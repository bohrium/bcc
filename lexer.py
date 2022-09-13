''' author: sam tenka
    change: 2022-09-12
    create: 2022-09-12
    descrp: Specify a finite state transducer (intended to lex )
            Given backus-naur grammar and string, return sequence of tokens.

            We specify a transducer via a "deterministic regular expression".
            This contrasts with traditional regex semantics, where a length-N
            regex determines a size-N nondeterministic finite automaton and
            hence a size-2^N deterministic finite automaton).

            Each regex matches greedily to its input string; it returns either
            None or a list of strings.  Our grammar for specifying regexes is:

                REGEX   =   STRING_LITERAL              -- hardcoded input (and output if match) 
                        |   CLASSES                     -- common input classes (output if match) 
                        |   "[" _ OUT_TOKEN _ "]"       -- hardcoded output 
                        |   "!" _ REGEX                 -- discard output   
                        |   REGEX _ "|" _ REGEX         -- disjunction 
                        |   REGEX _ REGEX               -- concatenation 
                        |   "*" _ REGEX                 -- kleene star (easier parsing with star at first)
                        |   "?" _ REGEX                 -- match this (and output) or empty string (and don't output)
                        |   "(" _ REGEX _ ")"           --  

                _       =   *WHITESPACE                 --  

                CLASSES =   "ALPHA"                     -- 26+26+1 letters with underscore 
                        |   "DIGIT"                     -- 10 digits 
                        |   "LOWER"                     -- 26 lowercase
                        |   "PRINT"                     -- (SYMBL|ALPHA|DIGIT) (no " or \ or _)
                        |   "SYMBL                      -- printable punctuation EXCEPT FOR " and \ and _
                        |   "UPPER"                     -- 26 uppercase
                        |   "QUOTE"                     -- special char: double quote
                        |   "BCKSL"                     -- special char: backslash 
                        |   "NEWLN"                     -- special char: newline
                        |   "SPACE"                     -- special char: one `space` character

            Watch out!  The order of terms in a disjunction matters!  
            (Our disjunction is associative but not commutative).

            For example, the following regular expression matches a number in
            decimal notation (e.g. "+0.5" or "1." or ".33" but not "."): 

                ("+"|"-"|"") (
                    DIGIT *DIGIT ?("."       *DIGIT) |
                  |                "." DIGIT *DIGIT     
                )

            The following regular expression splits at whitespace and haskell
            comments:

                (
                    ("\"" *("\\""|"\\\\"|"\\n"|" "|PRINT) "\"")         |
                    !(" "|"\n")[SPLIT]                                  |
                    !("--" ("\\"|"\""|" "|PRINT)* ("\n"|""))[SPLIT]     |
                    PRINT
                )

'''

from utils import color_print
from preprocess import preprocess_specs, Text

SYMBOLS = set('`~!@#$%^&*()-=+[]{}|;:\',<.>/?') # no underscore
class LexerGenerator:
    def __init__(self, specs, verbose=False):
        self.rule = Text(specs)
        #with open(specs_filename) as f:
        #    self.rule = Text(f.read())

        if verbose:
            print(self.specs)

        self.lexer = self.lexer_from_disjun(self.rule)

    def lexer_from_pred(self, charpred):
        def new_lexer(text):
            c = text.peek()
            if charpred(c):
                text.match(c)
                return [c]
            return None
        return new_lexer

    def lexer_from_disjun(self, rule):
        sublexers = []
        while not rule.is_at_end():
            sublexers.append(self.lexer_from_concat(rule))
            while rule.match(' ') or rule.match('\n'): pass
            if not rule.match('|'): break
                
        return self.build_disjun(sublexers)

    def lexer_from_concat(self, rule):
        sublexers = []
        while not rule.is_at_end() and rule.peek() not in ')|':
            sublexer = self.lexer_from_atom(rule)
            #if sublexer is None: break
            sublexers.append(sublexer)
            while rule.match(' ') or rule.match('\n'): pass

        return self.build_concat(sublexers) 

    def lexer_from_atom(self, rule):
        while rule.match(' ') or rule.match('\n'): pass
        
        #print('${}$'.format(rule.peek()))

        if rule.peek() == '(': 
            rule.match('(')
            while rule.match(' ') or rule.match('\n'): pass
            lexer = self.lexer_from_disjun(rule)
            while rule.match(' ') or rule.match('\n'): pass
            assert rule.match(')')
        elif rule.match('ALPHA'): lexer = self.lexer_from_pred(lambda c: c.isalpha() or c=='_')
        elif rule.match('DIGIT'): lexer = self.lexer_from_pred(lambda c: c.isdigit())
        elif rule.match('LOWER'): lexer = self.lexer_from_pred(lambda c: c.islower())
        elif rule.match('PRINT'): lexer = self.lexer_from_pred(lambda c: c.isalpha() or c=='_' or c.isdigit() or c in SYMBOLS)
        elif rule.match('SYMBL'): lexer = self.lexer_from_pred(lambda c: c in SYMBOLS)
        elif rule.match('UPPER'): lexer = self.lexer_from_pred(lambda c: c.isupper())
        elif rule.match('QUOTE'): lexer = self.lexer_from_pred(lambda c: c=='"')
        elif rule.match('BCKSL'): lexer = self.lexer_from_pred(lambda c: c=='\\')
        elif rule.match('NEWLN'): lexer = self.lexer_from_pred(lambda c: c=='\n')
        elif rule.match('SPACE'): lexer = self.lexer_from_pred(lambda c: c==' ')
        elif rule.peek() == '"':
            rule.match('"')
            lexer = self.build_in(rule.match_until('"'))   # TODO: escape?
            assert rule.match('"')
        elif rule.peek() == '[': 
            rule.match('[')
            lexer = self.build_out(rule.match_until(']'))  # TODO: escape?
            assert rule.match(']')
        elif rule.peek() == '!':
            rule.match('!')
            while rule.match(' ') or rule.match('\n'): pass
            lex = self.lexer_from_atom(rule)
            lexer = self.build_discard(lex)
        elif rule.peek() == '*':
            rule.match('*')
            while rule.match(' ') or rule.match('\n'): pass
            lex = self.lexer_from_atom(rule)
            lexer = self.build_kleene(lex)
        elif rule.peek() == '?':
            rule.match('?')
            while rule.match(' ') or rule.match('\n'): pass
            lex = self.lexer_from_atom(rule)
            lexer = self.build_maybe(lex)
        else:
            assert False

        return lexer

    def build_in(self, in_string):
        def new_lexer(text):
            m = text.match(in_string)
            return None if not m else list(in_string)
        return new_lexer

    def build_out(self, out_token):
        def new_lexer(text):
            return [out_token]
        return new_lexer

    def build_discard(self, kid ):
        def new_lexer(text):
            l = kid(text)
            return None if l is None else []
        return new_lexer

    def build_concat(self, kids):
        def new_lexer(text):
            l = []
            for k in kids:
                ll = k(text)
                if ll is None: return None
                l += ll 
            return l
        return new_lexer

    def build_disjun(self, kids):
        def new_lexer(text):
            for k in kids:
                ll = k(text)
                if ll is not None: 
                    return ll
            return None
        return new_lexer

    def build_kleene(self, kid ):
        def new_lexer(text):
            l = []
            while not text.is_at_end():
                ll = kid(text)
                if ll is None: break
                l += ll
            return l
        return new_lexer

    def build_maybe (self, kid ):
        def new_lexer(text):
            l = []
            if not text.is_at_end():
                ll = kid(text)
                if ll is None: return []
                l += ll
            return l
        return new_lexer

def tokenize_from_lex_stream(
        stream,
        init_state = ([], ''),
        transform = lambda c, s: (s[0], s[1]+c) if len(c)==1 else (s[0]+[(s[1], c)], ''),
        readout = lambda state: state[0],
    ):
    state = init_state
    for c in stream:
        state = transform (c, state)
    return readout(state)

def lex_stream_from_code(code):
    with open('bcc.lexspec') as f:
        LG = LexerGenerator(f.read())
    print('built lexer!')

    in_stream = LG.lexer(Text(code))
    out_stream = tokenize_from_lex_stream(in_stream)
    return out_stream 

if __name__ == '__main__':
    texts = (
        'moo->goo',
        'if(identifier==identifier){identifier=number-literal;}'                ,
        '[identifier:<identifier:[]>,identifier:identifier->mut(identifier)]'   ,
        '''
        --\\
        if h_while[&{_c_0w+4.6&]&}7>==>5-- moo
        "\\\\ if \\\" \\n why?"
        //comm''',
    )

    for code in texts:
        lex_stream = lex_stream_from_code(code)
        for token, label in lex_stream:
            print(repr(token), label)
        input('\nenter for next\n')
