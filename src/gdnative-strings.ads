with Ada.Finalization;

with GDNative.Thin;

package GDNative.Strings is

  -- I would name it String, but I don't want to confuse with the normal type

  type GString is new Ada.Finalization.Controlled with private;
  -- godot_string_destroy
  overriding procedure Finalize (Object : in out GString);

  function Ref (Item : in GString) return Thin.godot_string_const_ptr;

  -- godot_string_new_with_wide_string
  procedure Initialize (Item : in out GString; Value : in Wide_String);

  -- godot_string_new_with_copy
  procedure Copy (Dest : in out GString; Source : in GString);
  procedure Copy (Dest : in out GString; Source : in Thin.godot_string_const_ptr);

  -- TODO: godot_string_operator_index
  -- TODO: godot_string_operator_index_const
  
  -- godot_string_wide_str
  function Image (Item : in GString) return Wide_String;

  -- TODO: godot_string_operator_equal
  -- TODO: godot_string_operator_less
  -- TODO: godot_string_operator_plus

  -- godot_string_length
  function Length (Item : in GString) return Int_64_Unsigned;

  -- TODO: godot_string_new
  -- TODO: godot_string_casecmp_to
  -- TODO: godot_string_nocasecmp_to
  -- TODO: godot_string_naturalnocasecmp_to
  -- TODO: godot_string_begins_with
  -- TODO: godot_string_begins_with_char_array
  -- TODO: godot_string_bigrams
  -- TODO: godot_string_chr
  -- TODO: godot_string_ends_with
  -- TODO: godot_string_find
  -- TODO: godot_string_find_from
  -- TODO: godot_string_findmk
  -- TODO: godot_string_findmk_from
  -- TODO: godot_string_findmk_from_in_place
  -- TODO: godot_string_findn
  -- TODO: godot_string_findn_from
  -- TODO: godot_string_find_last
  -- TODO: godot_string_format
  -- TODO: godot_string_format_with_custom_placeholder
  -- TODO: godot_string_hex_encode_buffer
  -- TODO: godot_string_hex_to_int
  -- TODO: godot_string_hex_to_int_without_prefix
  -- TODO: godot_string_insert
  -- TODO: godot_string_is_numeric
  -- TODO: godot_string_is_subsequence_of
  -- TODO: godot_string_is_subsequence_ofi
  -- TODO: godot_string_lpad
  -- TODO: godot_string_lpad_with_custom_character
  -- TODO: godot_string_match
  -- TODO: godot_string_matchn
  -- TODO: godot_string_md5
  -- TODO: godot_string_num
  -- TODO: godot_string_num_int64
  -- TODO: godot_string_num_int64_capitalized
  -- TODO: godot_string_num_real
  -- TODO: godot_string_num_scientific
  -- TODO: godot_string_num_with_decimals
  -- TODO: godot_string_pad_decimals
  -- TODO: godot_string_pad_zeros
  -- TODO: godot_string_replace_first
  -- TODO: godot_string_replace
  -- TODO: godot_string_replacen
  -- TODO: godot_string_rfind
  -- TODO: godot_string_rfindn
  -- TODO: godot_string_rfind_from
  -- TODO: godot_string_rfindn_from
  -- TODO: godot_string_rpad
  -- TODO: godot_string_rpad_with_custom_character
  -- TODO: godot_string_similarity
  -- TODO: godot_string_sprintf
  -- TODO: godot_string_substr
  -- TODO: godot_string_to_double
  -- TODO: godot_string_to_float
  -- TODO: godot_string_to_int
  -- TODO: godot_string_camelcase_to_underscore
  -- TODO: godot_string_camelcase_to_underscore_lowercased
  -- TODO: godot_string_capitalize
  -- TODO: godot_string_char_to_double
  -- TODO: godot_string_char_to_int
  -- TODO: godot_string_wchar_to_int
  -- TODO: godot_string_char_to_int_with_len
  -- TODO: godot_string_char_to_int64_with_len
  -- TODO: godot_string_hex_to_int64
  -- TODO: godot_string_hex_to_int64_with_prefix
  -- TODO: godot_string_to_int64
  -- TODO: godot_string_unicode_char_to_double
  -- TODO: godot_string_get_slice_count
  -- TODO: godot_string_get_slice
  -- TODO: godot_string_get_slicec
  -- TODO: godot_string_split
  -- TODO: godot_string_split_allow_empty
  -- TODO: godot_string_split_floats
  -- TODO: godot_string_split_floats_allows_empty
  -- TODO: godot_string_split_floats_mk
  -- TODO: godot_string_split_floats_mk_allows_empty
  -- TODO: godot_string_split_ints
  -- TODO: godot_string_split_ints_allows_empty
  -- TODO: godot_string_split_ints_mk
  -- TODO: godot_string_split_ints_mk_allows_empty
  -- TODO: godot_string_split_spaces
  -- TODO: godot_string_char_lowercase
  -- TODO: godot_string_char_uppercase
  -- TODO: godot_string_to_lower
  -- TODO: godot_string_to_upper
  -- TODO: godot_string_get_basename
  -- TODO: godot_string_get_extension
  -- TODO: godot_string_left
  -- TODO: godot_string_ord_at
  -- TODO: godot_string_plus_file
  -- TODO: godot_string_right
  -- TODO: godot_string_strip_edges
  -- TODO: godot_string_strip_escapes
  -- TODO: godot_string_erase
  -- TODO: godot_string_ascii
  -- TODO: godot_string_ascii_extended
  -- TODO: godot_string_utf8
  -- TODO: godot_string_parse_utf8
  -- TODO: godot_string_parse_utf8_with_len
  -- TODO: godot_string_chars_to_utf8
  -- TODO: godot_string_chars_to_utf8_with_len
  -- TODO: godot_string_hash
  -- TODO: godot_string_hash64
  -- TODO: godot_string_hash_chars
  -- TODO: godot_string_hash_chars_with_len
  -- TODO: godot_string_hash_utf8_chars
  -- TODO: godot_string_hash_utf8_chars_with_len
  -- TODO: godot_string_md5_buffer
  -- TODO: godot_string_md5_text
  -- TODO: godot_string_sha256_buffer
  -- TODO: godot_string_sha256_text
  -- TODO: godot_string_empty
  -- TODO: godot_string_get_base_dir
  -- TODO: godot_string_get_file
  -- TODO: godot_string_humanize_size
  -- TODO: godot_string_is_abs_path
  -- TODO: godot_string_is_rel_path
  -- TODO: godot_string_is_resource_file
  -- TODO: godot_string_path_to
  -- TODO: godot_string_path_to_file
  -- TODO: godot_string_simplify_path
  -- TODO: godot_string_c_escape
  -- TODO: godot_string_c_escape_multiline
  -- TODO: godot_string_c_unescape
  -- TODO: godot_string_http_escape
  -- TODO: godot_string_http_unescape
  -- TODO: godot_string_json_escape
  -- TODO: godot_string_word_wrap
  -- TODO: godot_string_xml_escape
  -- TODO: godot_string_xml_escape_with_quotes
  -- TODO: godot_string_xml_unescape
  -- TODO: godot_string_percent_decode
  -- TODO: godot_string_percent_encode
  -- TODO: godot_string_is_valid_float
  -- TODO: godot_string_is_valid_hex_number
  -- TODO: godot_string_is_valid_html_color
  -- TODO: godot_string_is_valid_identifier
  -- TODO: godot_string_is_valid_integer
  -- TODO: godot_string_is_valid_ip_address

-------
private
-------

  type GString is new Ada.Finalization.Controlled with record
    Low : aliased Thin.godot_string;
  end record;

end;
