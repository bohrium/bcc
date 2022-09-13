''' author: samtenka
    change: 2022-09-12
    create: 2019-05-20
    descrp: simple processing before feeding into lexer and parser
'''

def preprocess_specs(text):
    assert '\t' not in text, "bcc forbids tab characters" 
    text = '\n'.join(ln.strip() for ln in text.split('\n'))
    text = '\n'.join((ln for ln in text.split('\n')
                         if not ln.startswith('--')))
    text = '\n\n'.join(' '.join(ln.split('\n')) for ln in text.split('\n\n'))
    text = '\n'.join(ln.strip() for ln in text.split('\n'))
    return text

class Text:
    '''
    '''

    def __init__(self, tokens, labels=None):
        tokens = list(tokens)
        labels = ['' for _ in tokens] if labels is None else labels
        assert len(tokens)==len(labels)
        self.tokens = list(tokens)
        self.labels = labels
        self.index = 0

    def peek(self):
        return self.tokens[self.index]

    def match(self, targets): 
        targets = list(targets)
        tokens = self.tokens[self.index : self.index+len(targets)]
        if tokens == targets:
            self.index += len(targets)
            return True
        return False

    def match_until(self, delim=' '):
        if type(delim)==str:
            cc = delim[:]
            delim = lambda c: c==cc
        word = ''
        while True:
            if self.is_at_end(): break
            c = self.peek()
            if delim(c): break
            word += c
            self.match([c])
        return word

    def match_label(self, label): 
        if self.is_at_end(): return False
        if self.labels[self.index] == label:
            self.index += 1
            return True
        return False

    def is_at_end(self):
        return self.index == len(self.tokens)

