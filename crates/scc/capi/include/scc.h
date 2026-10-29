#ifndef SCC_H
#define SCC_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#if defined(_WIN32) && !defined(BINDING_TEST)
#include <windows.h>
#endif

typedef struct SccSettings SccSettings;
typedef struct SccOutput SccOutput;
typedef struct SccResult SccResult;
typedef struct SccSourceRef SccSourceRef;

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
   * Creates new compilation settings.
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
   * Runs the compiler with the given settings.
   */
  scc_compile *compile;
  /**
   * Frees the memory allocated for the result.
   */
  scc_free_result *free_result;
  /**
   * Returns the output for a successful compilation and NULL otherwise.
   */
  scc_get_success *get_success;
  /**
   * Copies the error message and returns the number of bytes copied, excluding
   * the null terminator. On error, a nonempty buffer is null-terminated and the
   * message is truncated to fit. Returns zero without touching the buffer on
   * success or when buffer_size is zero. The buffer may be NULL when buffer_size
   * is zero; otherwise it must be writable for at least buffer_size bytes.
   */
  scc_copy_error *copy_error;
  /**
   * Returns the source reference at the given index.
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
   * Returns the name of the entity behind the reference.
   */
  scc_source_ref_name *source_ref_name;
  /**
   * Returns the name of the parent of the entity behind the reference.
   */
  scc_source_ref_parent_name *source_ref_parent_name;
  /**
   * Returns the path to a file where the entity behind the reference is
   * defined.
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
