#ifndef SCC_H
#define SCC_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#if defined(_WIN32) && !defined(BINDING_TEST)
#include <windows.h>
#endif

/**
 * Ownership contract for all API functions:
 *
 * - All handle arguments must be non-NULL and live. Settings mutations require
 *   exclusive access. Input strings must be valid NUL-terminated UTF-8; they are
 *   copied during the call and remain owned by the caller.
 * - settings_new returns owned settings. Setters only borrow them. compile
 *   consumes and frees settings on both success and failure: never reuse or free
 *   the settings pointer after calling compile.
 * - compile returns an owned result, which must be passed to free_result exactly
 *   once. Do not free API allocations with free() or another allocator.
 * - Outputs, source references, and string views are read-only borrows from the
 *   result, valid only until free_result. Do not modify or free them separately,
 *   or use them after freeing the result. Source reference functions require a
 *   reference obtained from the same output argument.
 *
 * copy_error's buffer is caller-owned; see its zero-size NULL exception below.
 */
typedef struct SccSettings SccSettings;
typedef struct SccOutput SccOutput;
typedef struct SccResult SccResult;
typedef struct SccSourceRef SccSourceRef;

/**
 * Borrowed UTF-8 bytes, not NUL-terminated. Read exactly len bytes. An empty view
 * may have a NULL str; do not dereference it when len is zero. The bytes remain
 * valid only until the owning result is passed to free_result. Do not modify or
 * free str; copying this struct does not extend the bytes' lifetime.
 */
typedef struct StrWithLen {
  const char *str;
  size_t len;
} StrWithLen;

enum SccSourceRefType {
  SCC_SOURCE_REF_TYPE_UNDEFINED = 0,
  SCC_SOURCE_REF_TYPE_CLASS = 1,
  SCC_SOURCE_REF_TYPE_FIELD = 2,
  SCC_SOURCE_REF_TYPE_FUNCTION = 3,
  SCC_SOURCE_REF_TYPE_ENUM = 4,
};

typedef SccSettings *scc_settings_new(const char *r6_dir);

typedef void scc_settings_set_custom_cache_file(SccSettings *settings,
                                                const char *cache_file);

typedef void scc_settings_set_output_cache_file(SccSettings *settings,
                                                const char *cache_file);

typedef void scc_settings_add_script_path(SccSettings *settings,
                                          const char *path);

typedef void scc_settings_disable_error_popup(SccSettings *settings);

typedef void scc_settings_register_never_ref_type(SccSettings *settings,
                                                  const char *type_name);

typedef void scc_settings_register_mixed_ref_type(SccSettings *settings,
                                                  const char *type_name);

typedef SccResult *scc_compile(SccSettings *settings);

typedef void scc_free_result(SccResult *result);

typedef SccOutput *scc_get_success(SccResult *result);

typedef size_t scc_copy_error(SccResult *result, char *buffer,
                              size_t buffer_size);

typedef SccSourceRef *scc_output_get_source_ref(SccOutput *output,
                                                size_t index);

typedef size_t scc_output_source_ref_count(SccOutput *output);

typedef uint8_t scc_source_ref_type(SccOutput *output, SccSourceRef *ref);

typedef bool scc_source_ref_is_native(SccOutput *output, SccSourceRef *ref);

typedef StrWithLen scc_source_ref_name(SccOutput *output, SccSourceRef *ref);

typedef StrWithLen scc_source_ref_parent_name(SccOutput *output,
                                              SccSourceRef *ref);

typedef StrWithLen scc_source_ref_path(SccOutput *output, SccSourceRef *ref);

typedef size_t scc_source_ref_line(SccOutput *output, SccSourceRef *ref);

typedef struct SccApi {
  /**
   * Creates owned compilation settings. r6_dir is copied and remains owned by
   * the caller. Pass the settings to compile exactly once to consume them.
   */
  scc_settings_new *settings_new;
  /**
   * Overrides the default cache file location.
   */
  scc_settings_set_custom_cache_file *settings_set_custom_cache_file;
  /**
   * Sets a custom output cache file location.
   *
   * Added in redscript 0.5.18. It will be null if the loaded library version is
   * older. The caller should do a null check if they want to maintain backward
   * compatibility.
   */
  scc_settings_set_output_cache_file *settings_set_output_cache_file;
  /**
   * Adds a path to be searched for scripts during compilation.
   */
  scc_settings_add_script_path *settings_add_script_path;
  /**
   * Disables the error popup that is shown when the compiler encounters errors.
   *
   * Added in redscript 0.5.28. It will be null if the loaded library version is
   * older. The caller should do a null check if they want to maintain backward
   * compatibility.
   */
  scc_settings_disable_error_popup *settings_disable_error_popup;
  /**
   * Registers a type as @neverRef.
   *
   * Added in redscript 1.0.0. It will be null if the loaded library version is
   * older. The caller should do a null check if they want to maintain backward
   * compatibility.
   */
  scc_settings_register_never_ref_type *settings_register_never_ref_type;
  /**
   * Registers a type as @mixedRef.
   *
   * Added in redscript 1.0.0. It will be null if the loaded library version is
   * older. The caller should do a null check if they want to maintain backward
   * compatibility.
   */
  scc_settings_register_mixed_ref_type *settings_register_mixed_ref_type;
  /**
   * Runs the compiler, consuming and freeing settings on both success and
   * failure. The settings pointer is invalid after this call. Returns an owned
   * result that the caller must release with free_result exactly once.
   */
  scc_compile *compile;
  /**
   * Consumes and frees a non-NULL result returned by compile, exactly once.
   * Invalidates all outputs, source references, and string views borrowed from
   * that result. None may be used afterward.
   */
  scc_free_result *free_result;
  /**
   * Returns a borrowed output for a successful compilation and NULL otherwise.
   * The output is read-only, must not be freed separately, and remains valid
   * only until the owning result is passed to free_result.
   */
  scc_get_success *get_success;
  /**
   * Copies the error message and returns the number of bytes copied, excluding
   * the null terminator. On error, a nonempty buffer is null-terminated and the
   * message is truncated to fit. Returns zero without touching the buffer on
   * success or when buffer_size is zero. The buffer may be NULL when buffer_size
   * is zero; otherwise it must be writable for at least buffer_size bytes.
   * The result is only borrowed; copied bytes belong to the caller and remain
   * valid after free_result.
   */
  scc_copy_error *copy_error;
  /**
   * Returns a borrowed, read-only source reference at the given index, which
   * must be less than output_source_ref_count(output). Do not free it separately;
   * it remains valid only until the owning result is passed to free_result.
   */
  scc_output_get_source_ref *output_get_source_ref;
  /**
   * Returns the total number of source references in the output.
   */
  scc_output_source_ref_count *output_source_ref_count;
  /**
   * Returns the type of entity behind the reference.
   */
  scc_source_ref_type *source_ref_type;
  /**
   * Returns whether the entity behind the reference is native.
   */
  scc_source_ref_is_native *source_ref_is_native;
  /**
   * Returns a borrowed string view of the entity name, valid only until the
   * owning result is passed to free_result. Do not modify or free its bytes.
   */
  scc_source_ref_name *source_ref_name;
  /**
   * Returns a borrowed string view of the parent name, valid only until the
   * owning result is passed to free_result. Do not modify or free its bytes.
   */
  scc_source_ref_parent_name *source_ref_parent_name;
  /**
   * Returns a borrowed string view of the source file path, valid only until
   * the owning result is passed to free_result. Do not modify or free its bytes.
   */
  scc_source_ref_path *source_ref_path;
  /**
   * Returns the line in the source code where the entity behind the reference
   * is defined.
   */
  scc_source_ref_line *source_ref_line;
} SccApi;

#if defined(_WIN32) && !defined(BINDING_TEST)

inline SccApi scc_load_api(HMODULE module) {
  SccApi api = {
      (scc_settings_new *)GetProcAddress(module, "scc_settings_new"),
      (scc_settings_set_custom_cache_file *)GetProcAddress(
          module, "scc_settings_set_custom_cache_file"),
      (scc_settings_set_output_cache_file *)GetProcAddress(
          module, "scc_settings_set_output_cache_file"),
      (scc_settings_add_script_path *)GetProcAddress(
          module, "scc_settings_add_script_path"),
      (scc_settings_disable_error_popup *)GetProcAddress(
          module, "scc_settings_disable_error_popup"),
      (scc_settings_register_never_ref_type *)GetProcAddress(
          module, "scc_settings_register_never_ref_type"),
      (scc_settings_register_mixed_ref_type *)GetProcAddress(
          module, "scc_settings_register_mixed_ref_type"),
      (scc_compile *)GetProcAddress(module, "scc_compile"),
      (scc_free_result *)GetProcAddress(module, "scc_free_result"),
      (scc_get_success *)GetProcAddress(module, "scc_get_success"),
      (scc_copy_error *)GetProcAddress(module, "scc_copy_error"),
      (scc_output_get_source_ref *)GetProcAddress(module,
                                                  "scc_output_get_source_ref"),
      (scc_output_source_ref_count *)GetProcAddress(
          module, "scc_output_source_ref_count"),
      (scc_source_ref_type *)GetProcAddress(module, "scc_source_ref_type"),
      (scc_source_ref_is_native *)GetProcAddress(module,
                                                 "scc_source_ref_is_native"),
      (scc_source_ref_name *)GetProcAddress(module, "scc_source_ref_name"),
      (scc_source_ref_parent_name *)GetProcAddress(
          module, "scc_source_ref_parent_name"),
      (scc_source_ref_path *)GetProcAddress(module, "scc_source_ref_path"),
      (scc_source_ref_line *)GetProcAddress(module, "scc_source_ref_line"),
  };
  return api;
}

#endif

#endif /* SCC_H */
