append([A|B], C, [A|D]):- append(B, C, D).
append([], A, A).
vater(hans, peter).
vater(peter, frank).
grossvater(A, C):- vater(A, B), vater(B, C).

