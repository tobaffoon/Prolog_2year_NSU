:- set_prolog_flag(double_quotes, chars).
% s_expression("(f x)", []).

s_expression1(Phrase) :- s_expression(Phrase, []).

s_expression    --> cl_atom.
s_expression    --> ['('], nempty_s_sequence, [')'].
s_expression    --> ['['], nempty_s_sequence, [']'].
s_expression    --> ['{'], even_s_sequence, ['}'].

nempty_s_sequence   --> s_expression.
nempty_s_sequence   --> s_expression, spacer_sequence, nempty_s_sequence.

even_s_sequence     --> [].
even_s_sequence     --> s_expression, spacer_sequence, s_expression.
even_s_sequence     --> s_expression, spacer_sequence, even_s_sequence, spacer_sequence, s_expression.

spacer_sequence     --> spacer.
spacer_sequence     --> spacer, spacer_sequence.

cl_atom     --> cl_integer.
cl_atom     --> cl_string.
cl_atom     --> identificator.
cl_atom     --> keyword. 

spacer  --> [','].
spacer  --> ['\s'].
spacer  --> ['\t'].
spacer  --> ['\n'].

special_symbol  --> ['+'].
special_symbol  --> ['-'].
special_symbol  --> ['>'].
special_symbol  --> ['<'].
special_symbol  --> ['='].
special_symbol  --> ['*'].
special_symbol  --> ['_'].


cl_integer  --> cl_digit.
cl_integer  --> cl_digit, cl_integer.

cl_digit    --> {char_type(Digit, digit)}, [Digit].

cl_string  --> ['\"'], cl_char_sequence, ['\"'].

cl_char_sequence       --> [].
cl_char_sequence       --> cl_char, cl_char_sequence.         % build string to the right

cl_char         --> 
    {char_type(Char, ascii)},
    [Char].

cl_letter       -->
    {char_type(Char, alpha)},
    [Char].

identificator   --> cl_letter, letter_identificator.
identificator   --> special_symbol, ssymbol_identificator.

letter_identificator  --> [].
letter_identificator  --> cl_letter, letter_identificator.
letter_identificator  --> special_symbol, letter_identificator.
letter_identificator  --> cl_integer, letter_identificator.

ssymbol_identificator   --> [].
ssymbol_identificator   --> special_symbol, ssymbol_identificator.
ssymbol_identificator   --> cl_integer, ssymbol_identificator.

keyword     --> [':'], identificator.


teacher_test :-
    s_expression1("{inc (-> :Int :Int)}"),
    s_expression1("{inc (-> :Int :Int), x :Str}"),
    s_expression1("{inc (-> :Int :Int), >0 (-> :Int :Bool)}"),
    s_expression1("{= (-> A (-> A :Bool))}"),
    s_expression1("(lambda x (inc x))"),
    s_expression1("((lambda x x) 10)"),
    s_expression1("[(concat x) (lambda x (inc (inc x)))]"),
    s_expression1("{head (-> (:List A) A), x (:List :Int)}").