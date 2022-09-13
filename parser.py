''' author: samuel
    change: 2022-09-12
    create: 2019-02-01
    descrp: Given backus-naur grammar and string, return parse tree.
'''

from utils import color_print
from preprocess import preprocess_specs, Text
from lexer import lex_stream_from_code

class ParseTree:
    ''' 
    '''
    def __init__(self, label='ROOT', kids=[], ignore=False, unroll=False):
        self.label = label
        self.kids = kids
        self.ignore = ignore 
        self.unroll = unroll 

    def get_source(self):
        return ''.join(k if type(k)==type('') else k.get_source() for k in self.kids)

    def width(self): 
        return len([k for k in self.kids if type(k)==type('') or not k.ignore])

    def relevant_kids(self):
        ''' no literal strings and no ignoreds '''
        return (k for k in self.kids if type(k)!=type('') and not k.ignore)

    def display(self, depth=0, delim='   :', collapse=False):
        '''
            `collapse` indicates whether to collapse unary vines down to
            leafmost node of vine 
        '''
        if self.ignore:
            return
        elif not self.unroll and (not collapse or self.width()!=1):
            source = self.get_source()
            if len(source)>64+3:
                source = source[:32] + '(WHITE)...(YELLOW)' + source[-32:]
            source = '(YELLOW)' + source + '(WHITE)'
            color_print(delim*depth + '(BLUE)' + self.label + '(WHITE)[' + source + ']')
            depth += 1
        for k in self.kids:
            if type(k)==type(''):
                color_print(delim*depth + '(RED)' + k + '(WHITE)')
            else:
                k.display(depth, delim, collapse)

#==============================================================================

class ParserGenerator:
    def __init__(self, specs, verbose=False):
        self.rules_by_symbol = {}
        self.ignore = set([])
        self.unroll = set([])

        for r in filter(None, specs.split('\n\n')):
            split_at = r.find('=') 
            symbol, rule = r[:split_at].strip(), r[split_at+1:].strip()
            if symbol[-1]=='*':
                symbol = symbol[:-1]
                self.ignore.add(symbol)
            elif symbol[-1]=='!':
                symbol = symbol[:-1]
                self.unroll.add(symbol)

            self.rules_by_symbol[symbol] = rule

        self.parsers = {s:(lambda text: False) for s in self.rules_by_symbol.keys()} 
        self.get_parser = lambda symbol: ( lambda text: self.parsers[symbol](text) )

        for symbol, rule in self.rules_by_symbol.items():
            if verbose: print(symbol, rule)
            self.parsers[symbol] = self.build_labeled(
                label = symbol,
                subparser = self.parser_from_disjunction(Text(rule)),
                ignore = symbol in self.ignore,
                unroll = symbol in self.unroll
            )

    def parser_from_disjunction(self, rule):
        subparsers = []
        while not rule.is_at_end():
            subparsers.append(self.parser_from_sequence(rule))
            while rule.match(' ') or rule.match('\n'): pass
            if not rule.match('|'): break
                
        return self.build_disjunction(subparsers)

    def parser_from_sequence(self, rule):
        subparsers = []
        while rule.match(' ') or rule.match('\n'): pass
        while not rule.is_at_end() and rule.peek() not in ')|':
            subparsers.append(self.parser_from_atom(rule))
            while rule.match(' ') or rule.match('\n'): pass

        return self.build_sequence(subparsers) 

    def parser_from_atom (self, rule):
        while rule.match(' ') or rule.match('\n'): pass

        if rule.match('('): 
            parser = self.parser_from_disjunction(rule)
            while rule.match(' ') or rule.match('\n'): pass
            assert rule.match(')')
        elif rule.match('"'):
            parser = self.build_literal_token(rule.match_until('"'))
            assert rule.match('"')
        elif rule.match('['):
            parser = self.build_literal_label(rule.match_until(']')) 
            assert rule.match(']') 
        elif rule.match('*'):
            parser = self.parser_from_atom(rule)
            parser = self.build_star(parser)
        elif rule.match('?'):
            parser = self.parser_from_atom(rule)
            parser = self.build_maybe(parser)
        elif rule.peek().isalpha():
            symbol = rule.match_until(lambda c: not (c.isalpha() or c.isdigit() or c=='_'))
            parser = self.get_parser(symbol)
        else:
            assert False

        return parser

    def build_labeled(self, label, subparser, ignore, unroll):
        def new_parser(text):
            kids = subparser(text)
            if kids is None: return None
            #return kids
            return ParseTree(label, kids, ignore, unroll) 
        return new_parser

    def build_disjunction(self, subparsers):
        def new_parser(text):
            for sp in subparsers:
                kids = sp(text) 
                if kids: return kids
            return None
        return new_parser

    def build_sequence(self, subparsers): 
        def new_parser(text):
            kids = []  
            for sp in subparsers:
                new_kids = sp(text) 
                if new_kids is None: return None
                if type(new_kids) != type([]): new_kids = [new_kids]
                kids += new_kids
            return kids
        return new_parser

    def build_maybe (self, subparser ):
        def new_parser(text):
            kids = subparser(text)
            if kids is None: return []
            return kids
        return new_parser

    def build_star(self, subparser ):
        def new_parser(text):
            kids = []
            while True:
                new_kids = subparser(text)
                if new_kids is None: break
                if type(new_kids) != type([]): new_kids = [new_kids]
                kids += new_kids
            return kids
        return new_parser

    def build_literal_token(self, literal): 
        def new_parser(text):
            if text.match([literal]):
                return [literal]
            return None
        return new_parser

    def build_literal_label(self, literal): 
        def new_parser(text):
            if text.is_at_end(): return None
            c = text.peek()
            if text.match_label(literal):
                return [c]
            return None
        return new_parser

if __name__ == '__main__':
    import sys
    
    if len(sys.argv) == 1:
        grammar_filenm, program_filenm = 'bcc.grammar', 'test.bcc'
    elif len(sys.argv) == 3:
        grammar_filenm, program_filenm = sys.argv[1:3]
    else:
        assert len(sys.argv) in [1, 3], "expect 0 or 2 command line arguments"

    with open(program_filenm) as f:
        code = f.read()
    lex_stream = lex_stream_from_code(code)
    tokens = [t for t,_ in lex_stream]
    labels = [l for _,l in lex_stream]
    cc = Text(tokens, labels)

    with open(grammar_filenm) as f:
        spec = f.read()
    pp = preprocess_specs(spec)
    PG = ParserGenerator(pp)
    print('built parser!')

    P = PG.parsers['MAIN']
    PT = P(cc)
    PT.display()

