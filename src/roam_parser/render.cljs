(ns roam-parser.render (:require [clojure.string]))

(def component-definitions {"query" {::arg-type :lisp}
                            "embed" {::arg-type :lisp}
                            "youtube" {::arg-type :textual}
                            "Δ" {::arg-type :hybrid}
                            "POMO" {::arg-type :none}
                            "roam/js" {::arg-type :none}})

(def delta-shorthand-re (js/RegExp. #"^(\d+(?:\.\d+)?)(?: *([-\+\*]) *(\d+(?:\.\d+)?))?$" ""))

(defn parse-float [^string string]
  (let [result (js/parseFloat string)]
    (if (js/isNaN result) nil result)))

(defn parse-delta-shorthand [^string string]
  (let [match (.exec delta-shorthand-re string)]
    (if (nil? match)
      nil
      (let [duration (parse-float (nth match 1))
            operation (case (nth match 2)
                        "-" -
                        "+" +
                        "/" /
                        "*" *)
            modifier (parse-float (nth match 3))]
        ))))

(def base-re (js/RegExp. #"^([\w-]+|\[\[([\w-]+)\]\])(?::(.+)?|$)" "s"))
(def form-re (js/RegExp. #"[^\s,]+" "g"))

(defn get-forms [^string string]
  (reduce conj [] (.match string form-re)))

(defn parse-render [^string string delimiters]
  (let [match (.exec base-re string)]
    (if (nil? match)
      nil
      (let [name (nth match 1)
            page-name (nth match 2)
            content (nth match 3)
            trimmed-content (when (not (nil? content)) (clojure.string/trim content))
            [comp-name
             linked?] (if (nil? page-name)
                        [name false]
                        [page-name true])
            definition (get component-definitions comp-name)
            unrecognised? (nil? definition)]
        (merge {::id comp-name
                ::linked? linked?
                ::unrecognised? unrecognised?}
               (case (::arg-type definition)
                 :none (when (not (clojure.string/blank? content))
                         {:parse-error {:err-type :warning
                                        :err-message "This component does not support arguments"}})
                 :textual {::argument-string trimmed-content}
                 :hybrid {::arguments nil}
                 {::arguments nil}))))))
