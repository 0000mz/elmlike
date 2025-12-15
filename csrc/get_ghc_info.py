import subprocess
import sys

def get_field(pkg, field):
    try:
        out = subprocess.check_output(['ghc-pkg', 'field', pkg, field], text=True).strip()
        if out.startswith(field + ":"):
            val = out[len(field)+1:].strip()
            return val.split()
        return []
    except:
        return []

pkgs = ['base', 'rts', 'ghc-prim', 'integer-gmp', 'ghc-internal', 'ghc-bignum']
lib_dirs = set()
include_dirs = set()
libs = []

for pkg in pkgs:
    dirs = get_field(pkg, 'library-dirs')
    for d in dirs:
        lib_dirs.add(d)

    inc_dirs = get_field(pkg, 'include-dirs')
    for i in inc_dirs:
        include_dirs.add(i)
    
    ls = get_field(pkg, 'hs-libraries')
    libs.extend(ls)
    
    extra = get_field(pkg, 'extra-libraries')
    libs.extend(extra)

# Print -L flags
for d in lib_dirs:
    print(f'-L{d}')

# Print -I flags
for i in include_dirs:
    print(f'-I{i}')

# Print -l flags
for l in libs:
    print(f'-l{l}')
