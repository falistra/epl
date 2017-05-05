#
# See paper Dan Ghica https://arxiv.org/abs/1702.01695
#
from efprob_dc import *

t = random_state(range(2))
q = random_pred(range(2))

# print("\nInstrument conditioning test")
# print( ((instr(q) >> t) / (yes_pred @ truth(range(2)))) % [0,1],
#        " equals ", t / q )
# print( ((instr(q) >> t) / (no_pred @ truth(range(2)))) % [0,1],
#        " equals ", t / ~q )

class CC:
    # CC stands for Channel-with-Condition
    def __init__(self, chan_with_cond):
        self.chan = chan_with_cond

    @classmethod
    def observe(self, pred):
        return CC( instr(pred) )

    @classmethod
    def unit(self, chan_without_cond):
        return CC( point_state(True, bool_dom).as_chan() @ chan_without_cond )

    @classmethod
    def idn(self, dom):
        return CC.unit(idn(dom))

    @classmethod
    def discard(self, dom):
        return CC.unit(discard(dom))

    @classmethod
    def new(self, state):
        return CC.unit(state.as_chan())

    def __repr__(self):
        return repr(self.chan)

    def __mul__(self, other):
        # Compute the Kleisli composition (other ; self).
        # NOTE: this is the reversed order of functional composition *
        # of channels.
        return CC( (and_chan @ idn(other.chan.cod[1:])) \
                   * (idn(bool_dom) @ other.chan) \
                   * self.chan )

    def __matmul__(self, other):
        # parallel composition self @ other
        return CC( (and_chan @ \
                    idn(self.chan.cod[1:]) @ \
                    idn(other.chan.cod[1:])) \
                   * \
                   (idn(bool_dom) @ \
                    swap(self.chan.cod[1:], bool_dom) @ \
                    idn(other.chan.cod[1:])) \
                   * \
                   (self.chan @ other.chan) )

    def smul(self, scalar):
        return CC(self.chan.smul(scalar))

    def __add__(self, other):
        return CC(self.chan + other.chan)

    def enforce(self, state):
        out = self.chan >> state
        out_dom = out.dom[1:]
        return (discard(bool_dom) @ idn(out_dom)) \
            >> (out / (yes_pred @ truth(out_dom)))


IDN = CC.idn
UNIT = CC.unit
CONVEX_SUM = convex_sum
DISCARD = CC.discard
OBSERVE = CC.observe
NEW = CC.new

def ASSIGN_VAL(point, dom):
    dom = asdom(dom)
    if dom.iscont:
        raise Exception("Cannot assing for continuous domains")
    return CC.unit(chan_fromklmap(lambda x: point_state(point, dom), dom, dom))

def ASSING_STATE(point, stat):
    dom = asdom(stat.dom)
    if dom.iscont:
        raise Exception("Cannot assing for continuous domains")
    return CC.unit(chan_fromklmap(lambda x: stat, dom, dom))

bool_to_num = Channel.from_states([State([1,0], range(2)),
                                   State([0,1], range(2))],
                                  bool_dom)

def IFTHENELSE(pred, cond_chan1, cond_chan2):
    if pred.dom != cond_chan1.chan.dom \
       or pred.dom != cond_chan2.chan.dom \
        or cond_chan1.chan.cod != cond_chan2.chan.cod:
        return Exception('Domain mismatch in if-then-else')
    print("Case channel",)
    print(case_channel(cond_chan1.chan, cond_chan2.chan))
    print("bool_to_num @ idn(pred.dom)",)
    print(bool_to_num @ idn(pred.dom))
    print("instr(pred)",)
    print(instr(pred))

    cc = CC(case_channel(cond_chan1.chan, cond_chan2.chan) \
              * (bool_to_num @ idn(pred.dom)) \
              * instr(pred))
    print("chan_with_cond",cc)
    return cc

def REPEAT(n):
    def rep(cc, m):
        if m < 1:
            raise Exception('Repeating must be done at least once')
        if m == 1:
            return cc
        return cc * rep(cc, m-1)
    return lambda cond_chan : rep(cond_chan, n)


def REPEATED_OBSERVE(preds, obs_to_pred_fun=None, pre_chan=None, post_chan=None):
    n = len(preds)
    if n < 1:
        raise Exception('Repeated observation must be done at least once')
    pred = preds[0] if obs_to_pred_fun is None else obs_to_pred_fun(preds[0])
    cr = OBSERVE(pred)
    if not pre_chan is None:
        cr = pre_chan * cr
    if not post_chan is None:
        cr = cr * post_chan
    if n == 1:
        return cr
    return cr * REPEATED_OBSERVE(preds[1:],
                                 obs_to_pred_fun = obs_to_pred_fun,
                                 pre_chan = pre_chan,
                                 post_chan = post_chan)


#
# random channels, states and predicates
#
c = cpt(0.2, 0.5, 0.7, 1)
d = cpt(0.3, 0.9)
s1 = random_state(bnd)
s2 = random_state(bnd)
p1 = random_pred(bnd)
p2 = random_pred(bnd)

disease_dom = ['D', '~D']
mood_dom = ['M', '~M']
w = State([0.05, 0.5, 0.4, 0.05], [disease_dom, mood_dom])

pred = point_pred('D', disease_dom) @ truth(mood_dom)
cond_chan1 = IDN(disease_dom) @ IDN(mood_dom) @ NEW(flip(9/10))
cond_chan2 = IDN(disease_dom) @ IDN(mood_dom) @ NEW(flip(1/20))

p3 = IFTHENELSE(point_pred('D', disease_dom) @ truth(mood_dom),
                IDN(disease_dom) @ IDN(mood_dom) @ NEW(flip(9/10)),
                IDN(disease_dom) @ IDN(mood_dom) @ NEW(flip(1/20))) \
        * \
        OBSERVE(truth(disease_dom) @ truth(mood_dom) @ yes_pred) \
        * \
        (DISCARD(disease_dom) @ IDN(mood_dom) @ DISCARD(bool_dom))
#print(p3)

disease_dom = ['D', '~D']
mood_dom = ['M', '~M']
predDirac = point_pred('D', disease_dom)
vero = truth(mood_dom)
# pred = point_pred('D', disease_dom) @ truth(mood_dom)
pred = predDirac @ vero
IDN_disease_dom = IDN(disease_dom)
#print("IDN_disease_dom-> ",IDN_disease_dom)
IDN_mood_dom = IDN(mood_dom)
#print("IDN_mood_dom-> ",IDN_mood_dom)
flip_9_10 = flip(9/10)
#print("flip_9_10-> ",flip_9_10)
new_flip_9_10 = NEW(flip(9/10))
#print("new_flip_9_10-> ",new_flip_9_10)
#cond_chan1 = IDN(disease_dom) @ IDN(mood_dom) @ NEW(flip(9/10))
cond_chan1 = IDN_disease_dom @ IDN_mood_dom @ new_flip_9_10
#print("cond_chan1-> ",cond_chan1)
cond_chan2 = IDN(disease_dom) @ IDN(mood_dom) @ NEW(flip(1/20))

#p4 = IFTHENELSE(pred,cond_chan1,cond_chan2)
#print(p4)
#p4_ = p4 * \
#        OBSERVE(truth(disease_dom) @ truth(mood_dom) @ yes_pred) \
#        * \
#        (DISCARD(disease_dom) @ IDN(mood_dom) @ DISCARD(bool_dom))
#print(p4_)
#print( p4_.enforce(w) )
