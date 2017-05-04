
# parsetab.py
# This file is automatically generated. Do not edit.
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'programleftL_A_PARENleftR_A_PARENleftCROSSleftSTARleftCONDITIONINGleftPIPEleftEQUALSleftANDleftPLUSrightUNEGATEEQUALS NAME STRING PROBABILITY_VALUE BIT VALIDITY CONDITIONING LPAREN RPAREN L_S_PAREN R_S_PAREN L_B_PAREN R_B_PAREN L_A_PAREN R_A_PAREN SEMICOL COLON COMMA PLUS STAR PIPE KET CROSS AND NEGATE MARGINAL PRINT CPT FLIP RANDOM_STATE UNIFORM_STATE COPY IDN SWAP TRUTH FALSITY TRUE FALSEprogram : stm program : stm SEMICOL programstm : PRINT LPAREN exp RPAREN stm : exp stm : varStore EQUALS expexp : LPAREN exp RPARENexp : varLoad\n            | validity\n            | conditioning\n            | state\n            | stateExp\n            | predicate\n            | predicateExp\n            | channel\n            | channelExp\n            | stateTransformation\n            | predicateTransformation\n            | builtin_function\n            varLoad : NAME  varStore : NAME validity : exp VALIDITY expconditioning : exp CONDITIONING expstateTransformation : exp R_A_PAREN exppredicateTransformation : exp L_A_PAREN expstate : L_S_PAREN stateBody R_S_PARENstateBody : stateElem PLUS stateBodystateBody : stateElemstateElem : PROBABILITY_VALUE PIPE nameOrBool optionKETnameOrBool : name\n                    | string\n                    | booleanname : NAMEstring : STRINGboolean : TRUE\n                | FALSEoptionKET : KET\n                | emptystateExp : exp MARGINAL dimensionSel\n     dimensionSel : L_S_PAREN dimensionSelBody R_S_PARENdimensionSelBody : dimensionSelElem COMMA dimensionSelBodydimensionSelBody : dimensionSelElemdimensionSelElem : BITpredicate : L_B_PAREN predicateBody R_B_PARENpredicateBody : predicateElem COMMA predicateBodypredicateBody : predicateElempredicateElem : nameOrBool COLON PROBABILITY_VALUEpredicateExp : exp CROSS exp\n                    | exp AND exp\n                    | exp PLUS exp\n                    | exp PIPE exp\n                    | PROBABILITY_VALUE STAR exp\n    predicateExp : NEGATE exp %prec UNEGATE channel : L_A_PAREN matrice COMMA domcod COMMA domcod R_A_PAREN  matrice : L_S_PAREN righe R_S_PAREN  righe  : riga COMMA righe righe  : riga  riga : L_S_PAREN riga_values R_S_PAREN  riga_values  : PROBABILITY_VALUE  riga_values  : PROBABILITY_VALUE COMMA riga_valueschannelExp : exp STAR exp\n     domcod : L_S_PAREN domcod_values R_S_PAREN  domcod_values : nameOrBool  domcod_values :  nameOrBool COMMA domcod_valuesbuiltin_function : FLIP LPAREN PROBABILITY_VALUE RPAREN\n                    | UNIFORM_STATE LPAREN domcod_values RPAREN\n                    | RANDOM_STATE LPAREN domcod_values RPAREN\n                    | FALSITY LPAREN domcod_values RPAREN\n                    | COPY LPAREN domcod_values RPAREN\n                    | IDN LPAREN domcod_values RPAREN\n                    | SWAP LPAREN domcod COMMA domcod RPAREN\n                    | CPT LPAREN riga_values RPAREN\n                    | TRUTH LPAREN domcod_values RPARENempty : '
    
_lr_action_items = {'PRINT':([0,34,],[3,3,]),'LPAREN':([0,3,4,23,25,26,27,28,29,30,31,32,33,34,35,38,39,41,42,43,44,45,46,47,48,62,],[4,35,4,4,66,67,68,69,70,71,72,73,74,4,4,4,4,4,4,4,4,4,4,4,4,4,]),'NAME':([0,4,21,23,34,35,38,39,41,42,43,44,45,46,47,48,62,67,68,69,70,71,74,92,94,109,127,],[19,37,58,37,19,37,37,37,37,37,37,37,37,37,37,37,37,58,58,58,58,58,58,58,58,58,58,]),'L_S_PAREN':([0,4,23,24,34,35,38,39,40,41,42,43,44,45,46,47,48,62,65,72,97,124,132,142,],[20,20,20,65,20,20,20,20,81,20,20,20,20,20,20,20,20,20,98,109,109,98,109,109,]),'L_B_PAREN':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,]),'PROBABILITY_VALUE':([0,4,20,23,34,35,38,39,41,42,43,44,45,46,47,48,62,66,73,91,95,98,135,],[22,22,51,22,22,22,22,22,22,22,22,22,22,22,22,22,22,101,111,51,120,111,111,]),'NEGATE':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,]),'L_A_PAREN':([0,4,5,7,8,9,10,11,12,13,14,15,16,17,18,19,23,34,35,36,37,38,39,41,42,43,44,45,46,47,48,62,63,76,77,78,79,80,82,83,84,85,86,87,88,89,90,93,96,125,126,128,129,130,131,134,136,137,151,152,],[24,24,47,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,24,24,24,47,-19,24,24,24,24,24,24,24,24,24,24,24,-52,47,-6,47,-22,-38,-47,-48,-49,-50,-60,-23,-24,47,-25,-43,-51,-64,-65,-66,-67,-68,-69,-71,-72,-39,-70,-53,]),'FLIP':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,]),'UNIFORM_STATE':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,]),'RANDOM_STATE':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,]),'FALSITY':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,]),'COPY':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,]),'IDN':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,]),'SWAP':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,]),'CPT':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,]),'TRUTH':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,]),'$end':([1,2,5,7,8,9,10,11,12,13,14,15,16,17,18,19,37,63,75,77,78,79,80,82,83,84,85,86,87,88,89,90,93,96,113,125,126,128,129,130,131,134,136,137,151,152,],[0,-1,-4,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,-19,-52,-2,-6,-21,-22,-38,-47,-48,-49,-50,-60,-23,-24,-5,-25,-43,-51,-3,-64,-65,-66,-67,-68,-69,-71,-72,-39,-70,-53,]),'SEMICOL':([2,5,7,8,9,10,11,12,13,14,15,16,17,18,19,37,63,77,78,79,80,82,83,84,85,86,87,88,89,90,93,96,113,125,126,128,129,130,131,134,136,137,151,152,],[34,-4,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,-19,-52,-6,-21,-22,-38,-47,-48,-49,-50,-60,-23,-24,-5,-25,-43,-51,-3,-64,-65,-66,-67,-68,-69,-71,-72,-39,-70,-53,]),'VALIDITY':([5,7,8,9,10,11,12,13,14,15,16,17,18,19,36,37,63,76,77,78,79,80,82,83,84,85,86,87,88,89,90,93,96,125,126,128,129,130,131,134,136,137,151,152,],[38,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,38,-19,-52,38,-6,38,-22,-38,-47,-48,-49,-50,-60,-23,-24,38,-25,-43,-51,-64,-65,-66,-67,-68,-69,-71,-72,-39,-70,-53,]),'CONDITIONING':([5,7,8,9,10,11,12,13,14,15,16,17,18,19,36,37,63,76,77,78,79,80,82,83,84,85,86,87,88,89,90,93,96,125,126,128,129,130,131,134,136,137,151,152,],[39,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,39,-19,-52,39,-6,39,-22,-38,39,-48,-49,-50,39,39,39,39,-25,-43,39,-64,-65,-66,-67,-68,-69,-71,-72,-39,-70,-53,]),'MARGINAL':([5,7,8,9,10,11,12,13,14,15,16,17,18,19,36,37,63,76,77,78,79,80,82,83,84,85,86,87,88,89,90,93,96,125,126,128,129,130,131,134,136,137,151,152,],[40,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,40,-19,-52,40,-6,40,-22,-38,-47,-48,-49,-50,-60,-23,-24,40,-25,-43,-51,-64,-65,-66,-67,-68,-69,-71,-72,-39,-70,-53,]),'CROSS':([5,7,8,9,10,11,12,13,14,15,16,17,18,19,36,37,63,76,77,78,79,80,82,83,84,85,86,87,88,89,90,93,96,125,126,128,129,130,131,134,136,137,151,152,],[41,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,41,-19,-52,41,-6,41,-22,-38,-47,-48,-49,-50,-60,41,41,41,-25,-43,-51,-64,-65,-66,-67,-68,-69,-71,-72,-39,-70,-53,]),'AND':([5,7,8,9,10,11,12,13,14,15,16,17,18,19,36,37,63,76,77,78,79,80,82,83,84,85,86,87,88,89,90,93,96,125,126,128,129,130,131,134,136,137,151,152,],[42,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,42,-19,-52,42,-6,42,42,-38,42,-48,-49,42,42,42,42,42,-25,-43,42,-64,-65,-66,-67,-68,-69,-71,-72,-39,-70,-53,]),'PLUS':([5,7,8,9,10,11,12,13,14,15,16,17,18,19,36,37,50,55,56,57,58,59,60,61,63,76,77,78,79,80,82,83,84,85,86,87,88,89,90,93,96,118,125,126,128,129,130,131,134,136,137,139,140,141,151,152,],[43,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,43,-19,91,-29,-30,-31,-32,-33,-34,-35,-52,43,-6,43,43,-38,43,43,-49,43,43,43,43,43,-25,-43,43,-73,-64,-65,-66,-67,-68,-69,-71,-72,-39,-28,-36,-37,-70,-53,]),'PIPE':([5,7,8,9,10,11,12,13,14,15,16,17,18,19,36,37,51,63,76,77,78,79,80,82,83,84,85,86,87,88,89,90,93,96,125,126,128,129,130,131,134,136,137,151,152,],[44,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,44,-19,92,-52,44,-6,44,44,-38,44,-48,-49,-50,44,44,44,44,-25,-43,44,-64,-65,-66,-67,-68,-69,-71,-72,-39,-70,-53,]),'STAR':([5,7,8,9,10,11,12,13,14,15,16,17,18,19,22,36,37,63,76,77,78,79,80,82,83,84,85,86,87,88,89,90,93,96,125,126,128,129,130,131,134,136,137,151,152,],[45,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,62,45,-19,-52,45,-6,45,-22,-38,45,-48,-49,-50,-60,45,45,45,-25,-43,-51,-64,-65,-66,-67,-68,-69,-71,-72,-39,-70,-53,]),'R_A_PAREN':([5,7,8,9,10,11,12,13,14,15,16,17,18,19,36,37,63,76,77,78,79,80,82,83,84,85,86,87,88,89,90,93,96,125,126,128,129,130,131,134,136,137,147,150,151,152,],[46,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,46,-19,-52,46,-6,46,-22,-38,-47,-48,-49,-50,-60,-23,46,46,-25,-43,-51,-64,-65,-66,-67,-68,-69,-71,-72,-39,-61,152,-70,-53,]),'EQUALS':([6,19,],[48,-20,]),'RPAREN':([7,8,9,10,11,12,13,14,15,16,17,18,36,37,55,56,57,58,59,60,61,63,76,77,78,79,80,82,83,84,85,86,87,88,90,93,96,101,102,103,104,105,106,107,110,111,112,125,126,128,129,130,131,134,136,137,145,146,147,148,151,152,],[-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,77,-19,-29,-30,-31,-32,-33,-34,-35,-52,113,-6,-21,-22,-38,-47,-48,-49,-50,-60,-23,-24,-25,-43,-51,125,126,-62,128,129,130,131,134,-58,136,-64,-65,-66,-67,-68,-69,-71,-72,-39,-63,151,-61,-59,-70,-53,]),'STRING':([21,67,68,69,70,71,74,92,94,109,127,],[59,59,59,59,59,59,59,59,59,59,59,]),'TRUE':([21,67,68,69,70,71,74,92,94,109,127,],[60,60,60,60,60,60,60,60,60,60,60,]),'FALSE':([21,67,68,69,70,71,74,92,94,109,127,],[61,61,61,61,61,61,61,61,61,61,61,]),'R_S_PAREN':([49,50,55,56,57,58,59,60,61,99,100,103,111,114,115,116,117,118,122,133,139,140,141,143,144,145,148,149,],[90,-27,-29,-30,-31,-32,-33,-34,-35,123,-56,-62,-58,137,-41,-42,-26,-73,143,147,-28,-36,-37,-57,-55,-63,-59,-40,]),'R_B_PAREN':([52,53,119,120,],[93,-45,-44,-46,]),'COMMA':([53,55,56,57,58,59,60,61,64,100,103,108,111,115,116,120,121,123,143,147,],[94,-29,-30,-31,-32,-33,-34,-35,97,124,127,132,135,138,-42,-46,142,-54,-57,-61,]),'COLON':([54,55,56,57,58,59,60,61,],[95,-29,-30,-31,-32,-33,-34,-35,]),'KET':([55,56,57,58,59,60,61,118,],[-29,-30,-31,-32,-33,-34,-35,140,]),'BIT':([81,138,],[116,116,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'program':([0,34,],[1,75,]),'stm':([0,34,],[2,2,]),'exp':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[5,36,63,5,76,78,79,82,83,84,85,86,87,88,89,96,]),'varStore':([0,34,],[6,6,]),'varLoad':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,]),'validity':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,]),'conditioning':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,]),'state':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,]),'stateExp':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,]),'predicate':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,]),'predicateExp':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,]),'channel':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,]),'channelExp':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,]),'stateTransformation':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,]),'predicateTransformation':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,]),'builtin_function':([0,4,23,34,35,38,39,41,42,43,44,45,46,47,48,62,],[18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,]),'stateBody':([20,91,],[49,117,]),'stateElem':([20,91,],[50,50,]),'predicateBody':([21,94,],[52,119,]),'predicateElem':([21,94,],[53,53,]),'nameOrBool':([21,67,68,69,70,71,74,92,94,109,127,],[54,103,103,103,103,103,103,118,54,103,103,]),'name':([21,67,68,69,70,71,74,92,94,109,127,],[55,55,55,55,55,55,55,55,55,55,55,]),'string':([21,67,68,69,70,71,74,92,94,109,127,],[56,56,56,56,56,56,56,56,56,56,56,]),'boolean':([21,67,68,69,70,71,74,92,94,109,127,],[57,57,57,57,57,57,57,57,57,57,57,]),'matrice':([24,],[64,]),'dimensionSel':([40,],[80,]),'righe':([65,124,],[99,144,]),'riga':([65,124,],[100,100,]),'domcod_values':([67,68,69,70,71,74,109,127,],[102,104,105,106,107,112,133,145,]),'domcod':([72,97,132,142,],[108,121,146,150,]),'riga_values':([73,98,135,],[110,122,148,]),'dimensionSelBody':([81,138,],[114,149,]),'dimensionSelElem':([81,138,],[115,115,]),'optionKET':([118,],[139,]),'empty':([118,],[141,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> program","S'",1,None,None,None),
  ('program -> stm','program',1,'p_program_singleC','eplParser.py',127),
  ('program -> stm SEMICOL program','program',3,'p_program_seqC','eplParser.py',131),
  ('stm -> PRINT LPAREN exp RPAREN','stm',4,'p_stm_print','eplParser.py',136),
  ('stm -> exp','stm',1,'p_stm_exp','eplParser.py',140),
  ('stm -> varStore EQUALS exp','stm',3,'p_stm_assign','eplParser.py',144),
  ('exp -> LPAREN exp RPAREN','exp',3,'p_exp_par','eplParser.py',148),
  ('exp -> varLoad','exp',1,'p_exp_state','eplParser.py',152),
  ('exp -> validity','exp',1,'p_exp_state','eplParser.py',153),
  ('exp -> conditioning','exp',1,'p_exp_state','eplParser.py',154),
  ('exp -> state','exp',1,'p_exp_state','eplParser.py',155),
  ('exp -> stateExp','exp',1,'p_exp_state','eplParser.py',156),
  ('exp -> predicate','exp',1,'p_exp_state','eplParser.py',157),
  ('exp -> predicateExp','exp',1,'p_exp_state','eplParser.py',158),
  ('exp -> channel','exp',1,'p_exp_state','eplParser.py',159),
  ('exp -> channelExp','exp',1,'p_exp_state','eplParser.py',160),
  ('exp -> stateTransformation','exp',1,'p_exp_state','eplParser.py',161),
  ('exp -> predicateTransformation','exp',1,'p_exp_state','eplParser.py',162),
  ('exp -> builtin_function','exp',1,'p_exp_state','eplParser.py',163),
  ('varLoad -> NAME','varLoad',1,'p_varLoad','eplParser.py',168),
  ('varStore -> NAME','varStore',1,'p_varStore','eplParser.py',172),
  ('validity -> exp VALIDITY exp','validity',3,'p_validity','eplParser.py',176),
  ('conditioning -> exp CONDITIONING exp','conditioning',3,'p_conditioning','eplParser.py',180),
  ('stateTransformation -> exp R_A_PAREN exp','stateTransformation',3,'p_stateTransformation','eplParser.py',184),
  ('predicateTransformation -> exp L_A_PAREN exp','predicateTransformation',3,'p_predicate_transformation','eplParser.py',188),
  ('state -> L_S_PAREN stateBody R_S_PAREN','state',3,'p_state_par','eplParser.py',192),
  ('stateBody -> stateElem PLUS stateBody','stateBody',3,'p_stateBody','eplParser.py',196),
  ('stateBody -> stateElem','stateBody',1,'p_stateBody_singleElem','eplParser.py',201),
  ('stateElem -> PROBABILITY_VALUE PIPE nameOrBool optionKET','stateElem',4,'p_state_elem','eplParser.py',205),
  ('nameOrBool -> name','nameOrBool',1,'p_nameOrBool','eplParser.py',209),
  ('nameOrBool -> string','nameOrBool',1,'p_nameOrBool','eplParser.py',210),
  ('nameOrBool -> boolean','nameOrBool',1,'p_nameOrBool','eplParser.py',211),
  ('name -> NAME','name',1,'p_name','eplParser.py',215),
  ('string -> STRING','string',1,'p_string','eplParser.py',219),
  ('boolean -> TRUE','boolean',1,'p_boolean','eplParser.py',223),
  ('boolean -> FALSE','boolean',1,'p_boolean','eplParser.py',224),
  ('optionKET -> KET','optionKET',1,'p_optionKET','eplParser.py',228),
  ('optionKET -> empty','optionKET',1,'p_optionKET','eplParser.py',229),
  ('stateExp -> exp MARGINAL dimensionSel','stateExp',3,'p_state_Exp','eplParser.py',232),
  ('dimensionSel -> L_S_PAREN dimensionSelBody R_S_PAREN','dimensionSel',3,'p_dimensionSel','eplParser.py',237),
  ('dimensionSelBody -> dimensionSelElem COMMA dimensionSelBody','dimensionSelBody',3,'p_dimensionSelBody','eplParser.py',241),
  ('dimensionSelBody -> dimensionSelElem','dimensionSelBody',1,'p_dimensionSelBody_singleElem','eplParser.py',246),
  ('dimensionSelElem -> BIT','dimensionSelElem',1,'p_dimensionSelElem','eplParser.py',250),
  ('predicate -> L_B_PAREN predicateBody R_B_PAREN','predicate',3,'p_predicate_par','eplParser.py',254),
  ('predicateBody -> predicateElem COMMA predicateBody','predicateBody',3,'p_predicateBody','eplParser.py',258),
  ('predicateBody -> predicateElem','predicateBody',1,'p_predicateBody_singleElem','eplParser.py',263),
  ('predicateElem -> nameOrBool COLON PROBABILITY_VALUE','predicateElem',3,'p_predicate_elem','eplParser.py',267),
  ('predicateExp -> exp CROSS exp','predicateExp',3,'p_predicate_exp','eplParser.py',271),
  ('predicateExp -> exp AND exp','predicateExp',3,'p_predicate_exp','eplParser.py',272),
  ('predicateExp -> exp PLUS exp','predicateExp',3,'p_predicate_exp','eplParser.py',273),
  ('predicateExp -> exp PIPE exp','predicateExp',3,'p_predicate_exp','eplParser.py',274),
  ('predicateExp -> PROBABILITY_VALUE STAR exp','predicateExp',3,'p_predicate_exp','eplParser.py',275),
  ('predicateExp -> NEGATE exp','predicateExp',2,'p_predicate_negate','eplParser.py',280),
  ('channel -> L_A_PAREN matrice COMMA domcod COMMA domcod R_A_PAREN','channel',7,'p_channel_def','eplParser.py',284),
  ('matrice -> L_S_PAREN righe R_S_PAREN','matrice',3,'p_matrice','eplParser.py',288),
  ('righe -> riga COMMA righe','righe',3,'p_righe','eplParser.py',292),
  ('righe -> riga','righe',1,'p_righe_singleRow','eplParser.py',297),
  ('riga -> L_S_PAREN riga_values R_S_PAREN','riga',3,'p_riga','eplParser.py',301),
  ('riga_values -> PROBABILITY_VALUE','riga_values',1,'p_riga_values_single','eplParser.py',305),
  ('riga_values -> PROBABILITY_VALUE COMMA riga_values','riga_values',3,'p_riga_values','eplParser.py',309),
  ('channelExp -> exp STAR exp','channelExp',3,'p_channel_Exp','eplParser.py',314),
  ('domcod -> L_S_PAREN domcod_values R_S_PAREN','domcod',3,'p_domcod','eplParser.py',319),
  ('domcod_values -> nameOrBool','domcod_values',1,'p_domcod_values_last','eplParser.py',323),
  ('domcod_values -> nameOrBool COMMA domcod_values','domcod_values',3,'p_domcod_values','eplParser.py',327),
  ('builtin_function -> FLIP LPAREN PROBABILITY_VALUE RPAREN','builtin_function',4,'p_builtin_function','eplParser.py',332),
  ('builtin_function -> UNIFORM_STATE LPAREN domcod_values RPAREN','builtin_function',4,'p_builtin_function','eplParser.py',333),
  ('builtin_function -> RANDOM_STATE LPAREN domcod_values RPAREN','builtin_function',4,'p_builtin_function','eplParser.py',334),
  ('builtin_function -> FALSITY LPAREN domcod_values RPAREN','builtin_function',4,'p_builtin_function','eplParser.py',335),
  ('builtin_function -> COPY LPAREN domcod_values RPAREN','builtin_function',4,'p_builtin_function','eplParser.py',336),
  ('builtin_function -> IDN LPAREN domcod_values RPAREN','builtin_function',4,'p_builtin_function','eplParser.py',337),
  ('builtin_function -> SWAP LPAREN domcod COMMA domcod RPAREN','builtin_function',6,'p_builtin_function','eplParser.py',338),
  ('builtin_function -> CPT LPAREN riga_values RPAREN','builtin_function',4,'p_builtin_function','eplParser.py',339),
  ('builtin_function -> TRUTH LPAREN domcod_values RPAREN','builtin_function',4,'p_builtin_function','eplParser.py',340),
  ('empty -> <empty>','empty',0,'p_empty','eplParser.py',344),
]