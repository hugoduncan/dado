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


(defn extract-search-replace-diffs
  "Extracts all search-replace diffs from text.
   Return their content as a string.
   Diff blocks are identified by markdown code fence markers.
   Returns empty string if no diff blocks found.
   Throws ex-info with :error/update-extraction if malformed blocks found."
  [text]
  (core/extract-search-replace-diffs text))
