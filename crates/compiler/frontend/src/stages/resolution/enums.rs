use super::*;

impl<'scope, 'ctx> NameResolution<'scope, 'ctx> {
    pub(super) fn process_enum(&mut self, entry: ParsedEnum<'ctx>) {
        let (enum_name, name_span) = entry.enum_.name;
        for (ann, ann_span) in &entry.meta.annotations {
            if ann.name == annotation::HINT {
                self.reporter
                    .report(Diagnostic::UnknownAnnotation(ann.name, *ann_span));
            }
        }

        let qs = entry.meta.qualifiers;
        if !qs.is_empty() {
            self.reporter
                .report(Diagnostic::UnusedItemQualifiers(qs, name_span));
        }

        let storage = entry.enum_.storage.as_ref().and_then(|(typ, span)| {
            let storage = match typ {
                ast::Type::Named { name, args } if args.is_empty() => {
                    ast::EnumStorage::from_name(name)
                }
                _ => None,
            };
            if storage.is_none() {
                self.reporter.report(Diagnostic::InvalidEnumStorage(*span));
            }
            storage
        });
        let mut by_name = IndexMap::default();
        let mut by_val = BTreeSet::<i64>::new();

        for (variant, span) in &entry.enum_.variants {
            let val = if let Some(val) = variant.value {
                val
            } else if let Some(last) = by_val.last() {
                if let Some(next) = last.checked_add(1) {
                    next
                } else {
                    self.reporter.report(Diagnostic::ValueOverflow(*span));
                    continue;
                }
            } else {
                0
            };

            if let Some(storage) = storage
                && !storage.contains(val)
            {
                self.reporter
                    .report(Diagnostic::EnumStorageOverflow(storage, val, *span));
                continue;
            }

            if by_name.insert(variant.name, val).is_some() {
                self.reporter.report(Diagnostic::DuplicateVariantName(
                    enum_name,
                    variant.name,
                    *span,
                ));
            }
            if !by_val.insert(val) {
                self.reporter.report(Diagnostic::DuplicateVariantValue(
                    enum_name,
                    variant.name,
                    val,
                    *span,
                ));
            }
        }

        let mut enum_ = Enum::new(by_name, Some(name_span));
        if let Some(storage) = storage {
            enum_ = enum_.with_storage(storage);
        }
        let schema = TypeSchema::Enum(enum_.into());
        self.symbols
            .add_type(entry.id, TypeDef::new([], schema, entry.meta.doc));
    }
}
