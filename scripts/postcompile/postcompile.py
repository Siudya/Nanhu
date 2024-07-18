import argparse
import os
import queue
import re
from assertion_alter import alter_print_info, print_queue
from functools import reduce
import concurrent.futures
from tqdm import tqdm
from datetime import date
import shutil

macro_pat = [
  re.compile(r".*sram_array_\dp\d+x\d+m\d+.*"),
  re.compile(r".*ClockGate(_\d*)?\.(v|sv)")
]

def macro_match(line:str) -> bool:
  hitVec = [(p.match(line) is not None) for p in macro_pat]
  return reduce(lambda a, b: a or b, hitVec)

def get_files(build_path:str) -> list[str]:
  files = list[str]()
  for f in os.scandir(build_path):
    file_path = os.path.join(build_path, f.path)
    if f.is_file() and (f.path.endswith(".sv") or f.path.endswith(".v")):
      files.append(file_path)
    elif f.is_dir():
      files += get_files(file_path)
  return files

this_dir = os.path.dirname(os.path.abspath(__file__))
cmd = "sed -i -E -f {} {}"

class RtlFile:
  file = ""
  vcs_style = True
  scr = ""

  def run(self):
    run_cmd = cmd.format(self.scr, self.file)
    os.system(run_cmd)

    buf = queue.Queue[str]()
    with open(self.file, "r") as f:
      file_lines = f.readlines()
      if(len(file_lines) == 0):
        print("{} failed!".format(self.file))
        exit()
      for line in file_lines:
        buf.put(line)
    q = alter_print_info(buf)
    print_queue(self.file, q)

  def __init__(self, file:str, vcs:bool):
    self.file = file
    if(vcs):
      self.scr = os.path.join(this_dir, "vcs.sed")
    else:
      self.scr = os.path.join(this_dir, "verilator.sed")

if __name__ == "__main__":
  parser = argparse.ArgumentParser(description='Post Compilation Script for XS')
  parser.add_argument('build', type=str, help='Build diretory')
  parser.add_argument('--vcs', action='store_true', help='VCS style assertion')
  parser.add_argument('-j', '--jobs', default=16, type=int, help='Parallel jobs', metavar='')
  parser.add_argument('--pack-release', action='store_true', help='Release all artifacts')
  parser.add_argument('--prefix', type=str, default="", help='Prefix for release')
  args = parser.parse_args()
  curdir = os.path.abspath(os.curdir)
  build_dir = os.path.join(curdir, args.build)
  release_base = f'{args.prefix}Nanhu-Release-{date.today().strftime("%b-%d-%Y")}'
  release_dir = os.path.join(curdir, release_base)
  pack = args.pack_release
  vcs = args.vcs
  jobs = args.jobs
  workerList = []

  for item in get_files(build_dir):
    workerList.append(RtlFile(item, vcs))

  print("Doing post-compiling procedures!")
  with concurrent.futures.ThreadPoolExecutor(jobs) as executor:
    results = concurrent.futures.as_completed([executor.submit(lambda x: x.run(), w) for w in workerList])
    list(tqdm(results, total=len(workerList)))

  if(pack):
    print(f"Making release package at {release_dir}!")
    if(os.path.exists(release_dir)):
      shutil.rmtree(release_dir)
    shutil.copytree(build_dir, release_dir)
    macros_dir = os.path.join(release_dir, "macros")
    top_flist = os.path.join(release_dir, "top.f")
    macros_flist = os.path.join(release_dir, "macros.f")
    os.makedirs(macros_dir)

    tf = open(top_flist, "w")
    mf = open(macros_flist, "w")
    tf.write("-f $release_dir/macros.f\n")
    for fn in get_files(release_dir):
      bn = os.path.basename(fn)
      if(macro_match(bn)):
        shutil.move(fn, os.path.join(macros_dir, bn))
        mf.write(f"$release_dir/macros/{bn}\n")
      else:
        tf.write(fn.replace(release_dir, "$release_dir") + "\n")
    tf.close()
    mf.close()

    gzfile = f"{release_base}.tar.gz!"
    if(os.path.exists(gzfile)):
      os.remove(gzfile)
    print(f"Packing {gzfile}!")
    pack_cmd = f"tar -zcf {release_base}.tar.gz {release_base}"
    os.system(pack_cmd)
