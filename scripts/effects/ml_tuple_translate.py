# Generate OCaml Tuple-Encoding translation for 
# all pairs of programs and properties that match on the name
#
# input: 
#   1. the path to drift executable
#   2. directory where to search for programs and properties
#   3. directory where to save the cps translated programs
 
import os.path
import glob
import subprocess

# file extensions for programs and properties
bench_prg_ext = ".ml"
bench_prop_ext = ".yml.prp"

def get_benches (src):
    def get_prg_and_prop (f):
        d = os.path.dirname(f)
        bench_prg = f
        bench = os.path.splitext(os.path.basename(f))[0]
        bench_prop = os.path.join(d, bench + bench_prop_ext)
        if os.path.exists(bench_prop):
            return (bench, bench_prg, bench_prop)
        else:
            return (bench, bench_prg, None)
        
    result = map (get_prg_and_prop, glob.glob(src + "/*" + bench_prg_ext))
    return result

def translate (drift_path, benches, dst):
    def tr_b (b, prg, prop):
        if (prop == None):
            return (b, "ERROR: prop file <" + b + bench_prop_ext + "> not found")
        else:
            tr_res = subprocess.run([drift_path, "-tuple-convert", "true", 
                                     "-ev-trans", "true",
                                     "-file", prg, "-prop", prop], 
                                    check=True, capture_output=True)
            fw = open(os.path.join(dst, "" + b + bench_prg_ext), "wb")
            fw.write(tr_res.stdout)
            fw.close()
            return (b, "Translated")
    i = 0
    result = []
    for (b, prg, prop) in benches: 
        result.append(tr_b (b, prg, prop))
    return result
        
# python3 cps_translate drift_path src_prog_dir dst_cps_path
if __name__ == "__main__":
    import sys
    drift_path = sys.argv[1]
    src = sys.argv[2]
    dst = sys.argv[3]
    benches = get_benches(src)
    result = translate (drift_path, benches, dst)
    print("\n".join(x[0] + ": " + x[1] for x in result))
    

