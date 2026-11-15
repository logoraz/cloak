(defpackage #:cloak/lib/jzon-message
  (:use #:cl)
  (:local-nicknames (#:jzon #:com.inuoe.jzon))
  (:export #:define-message
           #:decode-message)
  (:documentation "JSON message structs for the greetd protocol."))

(in-package #:cloak/lib/jzon-message)


;;; Code

(defvar *message-readers* (make-hash-table :test 'equal)
  "Maps a wire type string to the reader function for that message.")

(defun slot-spec-name (spec)
  "Return the slot name symbol of SPEC, a symbol or a list (SLOT . OPTIONS)."
  (if (consp spec) (first spec) spec))

(defun slot-spec-enum (spec)
  "Return the allowed values of SPEC as keywords, or NIL if not an enum.
SPEC is a symbol or a list (SLOT :ENUM (VALUE...)). For
(ERROR-TYPE :ENUM (AUTH-ERROR ERROR)) the result is (:AUTH-ERROR :ERROR)."
  (when (consp spec)
    (mapcar (lambda (value) (intern (symbol-name value) :keyword))
            (getf (rest spec) :enum))))

(defun slot-json-key (slot)
  "Return the JSON key string for SLOT, a symbol."
  (substitute #\_ #\- (string-downcase (symbol-name slot))))

(defun wire->enum (string allowed slot)
  "Return the keyword in ALLOWED that matches the wire STRING.
STRING is lower case with _ for hyphens; NIL gives NIL. A value that
matches nothing in ALLOWED signals an error naming SLOT and the value."
  (when string
    (let ((value (find-symbol
                  (string-upcase (substitute #\- #\_ string))
                  :keyword)))
      (if (member value allowed)
          value
          (error "Invalid value ~S for slot ~S; expected one of ~S"
                 string slot allowed)))))

(defun table-json-pairs (slots)
  "Return constructor arguments that read SLOTS from a variable TABLE.
For each slot spec in SLOTS, return its keyword and a form that fetches its
JSON key from TABLE. For (USERNAME) the result is
  (:USERNAME (GETHASH \"username\" TABLE))
An enum slot wraps the form in WIRE->ENUM, so its value is a keyword. This
is a flat list, ready to splice into a MAKE- call."
  (loop :for spec :in slots
        :for slot := (slot-spec-name spec)
        :for allowed := (slot-spec-enum spec)
        :for form := `(gethash ,(slot-json-key slot) table)
        :append (list (intern (symbol-name slot) :keyword)
                      (if allowed
                          `(wire->enum ,form ',allowed ',slot)
                          form))))

(defun enum->wire (keyword)
  "Return the wire string for the enum KEYWORD, in lower case with _
for hyphens. This reverses WIRE->ENUM."
  (substitute #\_ #\- (string-downcase (symbol-name keyword))))

(defun struct-table-forms (name slots)
  "Return forms that store the slots of NAME into a variable TABLE.
For each slot spec in SLOTS, return a form that sets its JSON key in TABLE
to the value of its accessor on a variable STRUCT, but only when that
value is not NIL. For NAME CREATE-SESSION and SLOTS (USERNAME) the
result is a list of one form:
  (WHEN (CREATE-SESSION-USERNAME STRUCT)
    (SETF (GETHASH \"username\" TABLE) (CREATE-SESSION-USERNAME STRUCT)))
An enum slot stores (ENUM->WIRE value), so the keyword is written as its
wire string. This list is ready to splice into the body of the writer."
  (loop :for spec :in slots
        :for slot := (slot-spec-name spec)
        :for accessor := (intern (format nil "~A-~A" name slot))
        :for value := (if (slot-spec-enum spec)
                          `(enum->wire (,accessor struct))
                          `(,accessor struct))
        :collect `(when (,accessor struct)
                    (setf (gethash ,(slot-json-key slot) table)
                          ,value))))

(defmacro define-message (name tag (&rest slots) &optional doc)
  "Define a greetd message struct NAME for the wire type TAG.
SLOTS are symbols or lists (SLOT :ENUM (VALUE...)). Each slot's JSON key is
its name in lower case with - replaced by _, so ERROR-TYPE is
\"error_type\". A slot that is NIL is left out when writing and is NIL when
a key is missing. An :ENUM slot holds a keyword, written to and read from
the wire as a string, so :AUTH-ERROR is \"auth_error\". A wire value outside
the list signals an error. The optional DOC becomes the struct docstring.

Generates:
  MAKE-NAME and accessors  (keyword constructor, no copier)
  HASH-TABLE->NAME         (reader, parsed table to struct)
  JSON->NAME               (parser, JSON string to struct)
  NAME->JSON               (writer, struct to JSON string)

and registers HASH-TABLE->NAME under TAG for DECODE-MESSAGE.

Example:
  (define-message error-reply \"error\"
    ((error-type :enum (auth-error error)) description)
    \"Indicates that the request failed.\")"

  (let ((make   (intern (format nil "MAKE-~A" name)))
        (reader (intern (format nil "HASH-TABLE->~A" name)))
        (parser (intern (format nil "JSON->~A" name)))
        (writer (intern (format nil "~A->JSON" name))))

    `(progn
       (defstruct (,name (:copier nil))
         ,@(when doc (list doc))
         ,@(mapcar #'slot-spec-name slots))

       (defun ,reader (table)
         "Build a message struct from TABLE, a parsed JSON object.
A key missing from TABLE gives NIL in its slot."
         (declare (ignorable table))
         (,make ,@(table-json-pairs slots)))

       (defun ,parser (string)
         "Parse the JSON STRING and build a message struct from it.
A key missing from the JSON gives NIL in its slot."
         (,reader (jzon:parse string)))

       ;; Registry
       ;; Each DEFINE-MESSAGE adds its reader here under its wire tag, so
       ;; DECODE-MESSAGE can choose the right struct from a "type" value.
       (setf (gethash ,tag *message-readers*) #',reader)

       (defun ,writer (struct)
         "Write STRUCT as a JSON string.
The object has a \"type\" key holding the wire tag, then one key for
each slot that is not NIL. A NIL slot is left out."
         (declare (ignorable struct))
         (let ((table (make-hash-table :test 'equal)))
           (setf (gethash "type" table) ,tag)
           ,@(struct-table-forms name slots)
           (jzon:stringify table))))))

(defun decode-message (string)
  "Parse the JSON STRING and return the message struct it describes.
The \"type\" key in the JSON picks the struct, using the readers that
DEFINE-MESSAGE registered. Signal an error naming the type if none is
registered."
  (let* ((table (jzon:parse string))
         (type (gethash "type" table))
         (reader (gethash type *message-readers*)))
    (if reader
        (funcall reader table)
        (error "Unknown greetd message type: ~S" type))))
