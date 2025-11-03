from retrieval.alias_resolver import resolve
samples=['APIPAY.cbl','apipay','TIM-360','screen.cpy','SCREEN','lonpf2','IRMAIN','IRMAlN','DAlLY']
for s in samples:
    print(f"{s} -> {resolve(s)}")
