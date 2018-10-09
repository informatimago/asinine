;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(defpackage #:asinine-parser
  (:use #:cl)
  (:nicknames #:asn1-parser)
  (:export #:parse-definition
           #:compile-definition))

(in-package #:asinine-parser)

(defun lisp-name (string)
  (with-output-to-string (s)
    (do ((i 0 (1+ i))
         (state :normal))
        ((= i (length string)))
      (let ((c (char string i)))
        ;; if the previous character was a downcased character and this one is an upcased character then
        ;; output a -
        (cond
          ((and (eq state :downcase) (upper-case-p c))
           (princ #\- s)
           (setf state :upcase))
          ((lower-case-p c)
           (setf state :downcase))
          (t (setf state :normal)))
        (princ (char-upcase c) s)))))

(defun lexer-wrapper (lexer)
  (lambda ()
     (multiple-value-bind (token value) (funcall lexer)
       (if (eq token 'name)
           (cond
             ((string= "ANY"             value) (values 'any             'any))
             ((string= "APPLICATION"     value) (values 'application     'application))
             ((string= "BEGIN"           value) (values 'begin           'begin))
             ((string= "BIT"             value) (values 'bit             'bit))
             ((string= "BOOLEAN"         value) (values 'boolean         'boolean))
             ((string= "BY"              value) (values 'by              'by))
             ((string= "CHOICE"          value) (values 'choicesym       'choicesym))
             ((string= "COMPONENTS"      value) (values 'components      'components))
             ((string= "DEFAULT"         value) (values 'defaultsym      'defaultsym))
             ((string= "DEFINED"         value) (values 'defined         'defined))
             ((string= "DEFINITIONS"     value) (values 'definitions     'definitions))
             ((string= "END"             value) (values 'end             'end))
             ((string= "EXPLICIT"        value) (values 'explicit        'explicit))
             ((string= "FROM"            value) (values 'from            'from))
             ((string= "IDENTIFIED"      value) (values 'identified      'identified))
             ((string= "IDENTIFIER"      value) (values 'identifier      'identifier))
             ((string= "IMPLICIT"        value) (values 'implicit        'implicit))
             ((string= "IMPORTS"         value) (values 'imports         'imports))
             ((string= "INTEGER"         value) (values 'integer         'integer))
             ((string= "MAX"             value) (values 'max             'max))
             ((string= "NULL"            value) (values 'null            'null))
             ((string= "OBJECT"          value) (values 'object          'object))
             ((string= "OCTET"           value) (values 'octet           'octet))
             ((string= "OF"              value) (values 'of              'of))
             ((string= "OPTIONAL"        value) (values 'optional        'optional))
             ((string= "PRESENT"         value) (values 'present         'present))
             ((string= "PRIVATE"         value) (values 'private         'private))
             ((string= "SEQUENCE"        value) (values 'sequence        'sequence))
             ((string= "SET"             value) (values 'setsym          'setsym))
             ((string= "SIZE"            value) (values 'size            'size))
             ((string= "STRING"          value) (values 'string          'string))
             ((string= "TAGS"            value) (values 'tags            'tags))
             ((string= "UNIVERSAL"       value) (values 'universal       'universal))
             ((string= "WITH"            value) (values 'with            'with))

             (t                             (values token        value)))
           (values token value)))))

(cl-lex:define-string-lexer asn1-lexer
  "--(.*?)--"                ;;      inline  comment
  "--(.*)\\\n"               ;; single line comments
  ("0x([0-9a-fA-F]+)"        (return (values 'constant (parse-integer (or $1 "") :radix 16))))
  ("[-]?[0-9]+"              (return (values 'constant (parse-integer $@))))
  ("\\\"([0-9a-fA-F]+)\\\""  (return (values 'constant (parse-integer (or $1 "") :radix 16))))
  ("[\\w-]+"                 (return (values 'name (alexandria:symbolicate (lisp-name $@)))))
  ("\\.\\.\\."               (return (values '|...|       '|...|)))
  ("\\.\\."                  (return (values '|..|        '|..|)))
  ("\\."                     (return (values '|.|         '|.|)))
  ("\\,"                     (return (values '|,|         '|,|)))
  ("\\;"                     (return (values '|;|         '|;|)))
  ("\\:\\:\\="               (return (values '|::=|       '|::=|)))
  ("\\{"                     (return (values '|{|         '|{|)))
  ("\\}"                     (return (values '|}|         '|}|)))
  ("\\("                     (return (values '|(|         '|(|)))
  ("\\)"                     (return (values '|)|         '|)|)))
  ("\\["                     (return (values '|[|         '|[|)))
  ("\\]"                     (return (values '|]|         '|]|)))
  ("\\|"                     (return (values 'pipe        'pipe))))

(defun test-lexer (string)
  (let ((l (lexer-wrapper (asn1-lexer string))))
    (do (done)
        (done)
      (multiple-value-bind (token val) (funcall l)
        (if token
            (format t "~S ~S~%" token val)
            (setf done t))))))

;; http://www.it.kau.se/cs/education/courses/dvgc02/08p4/labs/asn1_bnf.txt
(yacc:define-parser *asn1-parser*
  (:start-symbol module-definition)
  (:terminals (|::=| |,| |.| |;| |{| |}| |(| |)| |..|  |...| |[| |]| pipe
                     integer boolean bit octet string any null
                     object identifier defaultsym
                     implicit explicit begin end definitions
                     sequence setsym of tags choicesym optional defined identified by
                     application universal private
                     name constant
                     size max
                     imports from
                     with components present))

  (module-definition
   (name maybe-oid-list definitions               |::=| begin import-list module-body end (lambda (a b c d e imp f g)     (declare (ignore c d e g))      `(:module ,a ,f :oid ,b :imports ,imp)))
   (name maybe-oid-list definitions explicit tags |::=| begin import-list module-body end (lambda (a b c d e f g imp h i) (declare (ignore c d e f  g i)) `(:module ,a ,h :oid ,b :explicit t :imports ,imp)))
   (name maybe-oid-list definitions implicit tags |::=| begin import-list module-body end (lambda (a b c d e f g imp h i) (declare (ignore c d e f g i))  `(:module ,a ,h :oid ,b :implicit t :imports ,imp))))

  (maybe-oid-list
   (|{| object-identifier-list |}|
        (lambda (a b c) (declare (ignore a c)) b))
   empty)

  (import-list
   (imports import-froms |;| (lambda (a b c) (declare (ignore a c)) (list b)))
   empty)

  (import-froms
   (import-from (lambda (a) (list a)))
   (import-froms import-from (lambda (a b) (append a (list b)))))

  (import-from
   (imported-names from name name-or-oid (lambda (names from name name-or-oid) (declare (ignore from)) (list names name name-or-oid))))

  (imported-names
   (imported-name                    (lambda (a)                          (list a)))
   (imported-names |,| imported-name (lambda (a b c) (declare (ignore b)) (append a (list c)))))

  (imported-name
   (name         (lambda (name)                            `(:imported-name ,name)))
   (name |{| |}| (lambda (name b c) (declare (ignore b c)) `(:imported-name-bracket ,name))))

  (name-or-oid
   name
   oid)

  (oid
   (|{| object-identifier-list |}| (lambda (a b c) (declare (ignore a c)) (print `(oid ,b)) b)))

  (module-body
   assignment-list
   empty)

  (assignment-list
   (assignment (lambda (a) (list a)))
   (assignment-list assignment (lambda (a b) (append a (list b)))))

  (assignment
   (name                   |::=| type                           (lambda (a b c)         (declare (ignore b))      `(,a ,c)))
   (name object identifier |::=| name-or-oid                    (lambda (a b c d e)     (declare (ignore b c d))  `(,a (:object-identifier ,e))))
   (name integer           |::=| constant                       (lambda (a b c d)       (declare (ignore b c))    `(,a (:integer ,d))))

   (name name              |::=| |{| object-identifier-list/mbib maybe-oid |}|
         (lambda (n1 n2 c l oidl maybe-oid r)
           (print `(oidl ,oidl maybe-oid ,maybe-oid))
           (if maybe-oid
               (if (and (= 3 (length oidl))
                        (eq 'identified (second oidl))
                        (eq 'by         (third oidl)))
                   `(,n1 (:type-identified-by ,n2 ,(first oidl) ,maybe-oid))
                   (error "Invalid syntax ~S" (list n1 n2 c l oidl maybe-oid r)))
               `(,n1 (:object-identifier-alias ,n2 ,oidl))))))

  (maybe-oid
   oid
   name
   empty)

  (object-identifier-list/mbib
   (oid               (lambda (a) (list a)))
   (|...|             (lambda (a) (list a)))
   (named-number/mbib (lambda (a) (list a)))
   (object-identifier-list/mbib named-number/mbib (lambda (a b) (append a (list b)))))

  (named-number/mbib
   (name |(| constant |)| (lambda (a b c d) (declare (ignore b d)) `(:number ,a ,c)))
   (name                  (lambda (a) `(:number ,a nil)))
   (constant              (lambda (a) `(:number nil ,a)))
   identified
   by
   primitive-type)

  (type
   external-type
   builtin-type
   defined-type)

  (external-type
   (name |.| name (lambda (a b c) (declare (ignore b)) `(:external-type ,a ,c))))

  (builtin-type
   primitive-type
   constructed-type
   tagged-type)

  (defined-type
    name
    (name |(| name |)|                   (lambda (a b c d) (declare (ignore b c d)) a))
    (name |(| bit-string-option-list |)| (lambda (a b c d) (declare (ignore b c d)) a))
    (name |{| object-identifier-list/mbib |}| (lambda (name l oidl r) (declare (ignore l r)) `(:defined-type ,name ,oidl))))

  (primitive-type
   integer-expr
   boolean
   bitstr
   octetstr
   any
   null
   objid)

  (constructed-type
   sequence-expr
   sequence-of-expr
   set
   set-of
   choice)

  (tagged-type
   (tag type (lambda (a b) `(:tagged-type ,a ,b)))
   (tag implicit type (lambda (a b c) (declare (ignore b)) `(:tagged-type ,a ,c :implicit t)))
   (tag explicit type (lambda (a b c) (declare (ignore b)) `(:tagged-type ,a ,c :explicit t))))

  (integer-expr
   (integer (lambda (a) (declare (ignore a)) `(:integer)))
   (integer |(| constant |..| constant |)|
            (lambda (a b c d e f) (declare (ignore a b d f)) `(:integer :range :start ,c :end ,e)))
   (integer |(| constant |..| name |)|
            (lambda (a b c d e f) (declare (ignore a b d f))
              `(:integer :range :start ,c :end ,e)))
   (integer |(| constant-list |)| (lambda (a b c d) (declare (ignore a b d)) `(:integer :member ,c)))
   (integer |{| named-number-list |}| (lambda (a b c d) (declare (ignore a b d)) `(:integer :member ,c)))
   (integer |{| named-number-list |}| |(| constant |..| name |)|
            (lambda (a b c d e f g h i) (declare (ignore a b d e g i))
              `(:integer :member ,c :start ,f :end ,h))))

  (constant-list
   (constant (lambda (a) (list a)))
   (constant-list pipe constant (lambda (a b c) (declare (ignore b)) (append a (list c)))))

  (bit-string-range
   (constant |..| constant (lambda (a b c) (declare (ignore b)) `(:range :start ,a :end ,c)))
   (constant |..| max (lambda (a b c) (declare (ignore b c)) `(:range :start ,a :end nil)))
   (constant |..| name (lambda (a b c) (declare (ignore b))
                         `(:range :start ,a :end ,c)))
   (constant (lambda (a) `(:integer :member (,a))))
   (name (lambda (a) `(:integer :member (,a)))))

  (bit-string-option
   (size |(| bit-string-range |)| (lambda (a b c d) (declare (ignore a b d)) c)))

  (bit-string-option-list
   (bit-string-option (lambda (a) (list a)))
   (bit-string-option-list |,| bit-string-option (lambda (a b c) (declare (ignore b)) (append a (list c)))))

  (bitstr
   (bit string (lambda (a b) (declare (ignore a b)) `(:bit-string)))
   (bit string |{| named-bit-list |}|
        (lambda (a b c d e) (declare (ignore a b c e)) `(:bit-string ,d)))
   (bit string |(| bit-string-option-list |)| (lambda (a b c d e) (declare (ignore a b c e)) `(:bit-string :options ,d))))

  (octetstr
   (octet string (lambda (a b) (declare (ignore a b)) :octet-string)))

  (objid
   (object identifier (lambda (a b) (declare (ignore a b)) :object-identifier)))

  (object-identifier-list
   (named-number (lambda (a) (list a)))
   (object-identifier-list named-number (lambda (a b) (append a (list b)))))

  (sequence-expr
   (sequence |{| element-type-list |}| constraint (lambda (a b c d constraint) (declare (ignore a b d)) `(:sequence ,c ,@(when constraint (list constraint)))))
   (sequence |{|                   |}| constraint (lambda (a b c   constraint) (declare (ignore a b c)) `(:sequence    ,@(when constraint (list constraint))))))

  (constraint
   (|(| constraint-disjonction |)| (lambda (l cd r) (declare (ignore l r)) cd))
   empty)

  (constraint-disjunction
   simple-constraint
   (constraint-disjunction pipe simple-constraint (lambda (cd pipe sc) (declare (ignore pipe)) `(:or ,cd ,sc))))

  (simple-constraint
   (with components |{| component-constraints |}| (lambda (w c l cc r) (declare (ignore w c l r)) cc)))

  (component-constraints
   |...|
   (name present (lambda (name constraint) `(:constraint ,name ,constraint))))

  (sequence-of-expr
   (sequence of type (lambda (a b c) (declare (ignore a b)) `(:sequence-of ,c)))
   (sequence bit-string-option of type
             (lambda (a b c d) (declare (ignore a c))
               `(:sequence-of ,d :size ,b))))

  (set
   (setsym |{| element-type-list |}| (lambda (a b c d) (declare (ignore a b d)) `(:set ,c)))
   (setsym |{| |}| (lambda (a b c) (declare (ignore a b c)) `(:set nil))))

  (set-of
   (setsym of type (lambda (a b c) (declare (ignore a b)) `(:set-of ,c)))
   (setsym bit-string-option-list of type
           (lambda (a b c d) (declare (ignore a c))
             `(:set-of ,d :options ,b))))

  (choice
   (choicesym |{| alternative-type-list |}| (lambda (a b c d) (declare (ignore a b d)) `(:choice ,c))))

  (tag
   (|[| class constant |]| (lambda (a b c d) (declare (ignore a d)) `(,b ,c))))

  (class
   (universal (lambda (a) (declare (ignore a)) :universal))
   (application (lambda (a) (declare (ignore a)) :application))
   (private (lambda (a) (declare (ignore a)) :private))
   empty)

  (named-number-list
   (named-number (lambda (a) (list a)))
   (named-number-list |,| named-number (lambda (a b c) (declare (ignore b)) (append a (list c)))))

  (named-number
   (name |(| constant |)| (lambda (a b c d) (declare (ignore b d)) `(:number ,a ,c)))
   (constant (lambda (a) `(:number nil ,a)))
   (name (lambda (a) `(:number ,a nil))))

  (named-bit-list
   (named-bit (lambda (a) (list a)))
   (named-bit-list |,| named-bit (lambda (a b c) (declare (ignore b)) (append a (list c)))))

  (named-bit
   (name |(| constant |)| (lambda (a b c d) (declare (ignore b d)) `(:named-bit ,a ,c))))

  (element-type-list
   (element-type (lambda (a) (list a)))
   (element-type-list |,| element-type (lambda (a b c) (declare (ignore b)) (append a (list c)))))

  (value
   name
   constant)

  (element-type
   (|...|        (lambda (a) (list a)))
   named-type
   (named-type optional (lambda (a b) (declare (ignore b)) (append a `(:optional t))))
   (named-type defaultsym value (lambda (a b c) (declare (ignore b)) (append a `(:default ,c))))
   (named-type defined by name
               (lambda (a b c d) (declare (ignore b c))
                 (append a `(:defined-by ,d))))
   (named-type defined by name optional
               (lambda (a b c d e) (declare (ignore b c e))
                 (append a `(:defined-by ,d :optional t)))))

  (named-type
   (name type)
   (name |[| constant |]| type (lambda (a b c d e) (declare (ignore b d)) `(,a ,e :tag ,c)))
   (name |[| constant |]| implicit type
         (lambda (a b c d e f) (declare (ignore b d e))
           `(,a ,f :tag ,c :implicit t)))
   (name |[| constant |]| explicit type
         (lambda (a b c d e f) (declare (ignore b d e))
           `(,a ,f :tag ,c :explicit t)))
   (type (lambda (a) `(nil ,a))))

  (alternative-type-list
   (named-type (lambda (a) (list a)))
   (alternative-type-list |,| named-type (lambda (a b c) (declare (ignore b)) (append a (list c)))))

  (empty))

(defun test-parser (string)
  (yacc:parse-with-lexer (lexer-wrapper (asn1-lexer string)) *asn1-parser*))

(defun parse-definition (pathspec)
  "Parse the ASN.1 specification stored in the file named by PATHSPEC. Returns the parsed definition."
  (let ((body
          (with-open-file (f pathspec :direction :input)
            (with-output-to-string (s)
              (do ((l (read-line f nil nil) (read-line f nil nil)))
                  ((null l))
                (princ l s)
                (fresh-line s))))))
    (let ((asn1 (test-parser body)))
      asn1)))

(defun compile-definition (pathspec &optional outfile)
  "Parse an ASN.1 definition and generate a Lisp file with functions to encode/decode using DER.

PATHSPEC ::= the ASN.1 definition.
OUTFILE ::= name of file to put the Lisp code into, defaults to the ASN.1 definition with the extension .lisp.

Returns the parsed contents."
  (let ((*package* (find-package "ASININE"))
        (pathname (or outfile
                      (merge-pathnames (make-pathname :type "lisp")
                                       (truename pathspec)))))
    (with-open-file (f pathname :direction :output :if-exists :supersede)
      (let ((asn1 (parse-definition pathspec)))
        (asinine:gen asn1 f)
        asn1))))
