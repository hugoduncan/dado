(ns dado.update-extractor.interface
  "Component for extracting update blocks from text content.
   Initially focused on diff extraction only."
  (:require [dado.update-extractor.core :as core]))

(defn extract-simplified-diffs
  "Extracts all simlified diffs from text.
   Return their content as a string.
   Diff blocks are identified by markdown code fence markers.  Returns empty
   string if no diff blocks found.  Throws ex-info with :error/update-extraction
   if malformed blocks found."
  [text]
  (core/extract-simplified-diffs text))


(defn extract-file-operation-directives
  "Extracts all file operation directives from text.
   Return their content as a string.
   Diff blocks are identified by markdown code fence markers.
   Returns empty string if no diff blocks found.
   Throws ex-info with :error/update-extraction if malformed blocks found."
  [text]
  (core/extract-file-operation-directives text))

(defn extract-updated-namespaces
  "Extracts all updated namespace lists from text.
   Returns sequence of namespace strings.
   Namespace lists are identified by markdown code fence markers with
   'updated-namespaces' language.
   Returns empty sequence if no namespace lists found.
   Throws ex-info with :error/update-extraction if malformed blocks found."
  [text]
  (core/extract-updated-namespaces text))
