#fdice = efp.State([0.167,0.167,0.167,0.167,0.167,0.167],["_1","_2","_3","_4","_5","_6"])
fdice = efp.uniform_state(["_1","_2","_3","_4","_5","_6"])
#fdice = efp.State([0.167,0.167,0.167,0.167,0.167,0.167],["_1","_2","_3","_4","_5","_6"])
even = efp.Predicate([0.0,1.0,0.0,1.0,0.0,1.0],["_1","_2","_3","_4","_5","_6"])
print(fdice / even)
print(fdice >= even)
