import os
import argparse
import re
import queue

def match(line, pattern_c):
  res = pattern_c.search(line.strip())
  return not (res == None)

def gen_prefix(str):
  first_letter = str.lstrip()[0]
  return str[0:str.find(first_letter):1]

def gen_spaces(str):
  first_letter = str.lstrip()[0]
  return str[0:str.find(first_letter)-2:1]

def alter_print_info(file_queue):
  rex_assert_begin = re.compile("\$error\(")
  rex_assert_body = re.compile("Assertion failed")
  rex_assert_end = re.compile("\);")
  assertion_queue = queue.Queue()
  res_queue = queue.Queue()

  while(True):
    line = file_queue.get()
    is_single_line = match(line, rex_assert_begin) and match(line, rex_assert_end)
    is_begin = match(line, rex_assert_begin)
    if(is_begin):
      res_queue.put(gen_spaces(line) + "begin\n")
      if(is_single_line):
        res_queue.put(gen_prefix(line) + "$fwrite(32'h80000002, \"Assertion failed: %m @ %t\", $time);\n")
        line = line.replace("Assertion failed:", "").replace("$error(", "$fwrite(32'h80000002, ")
        res_queue.put(line)
        res_queue.put(gen_spaces(line) + "end\n")
      else:
        if(match(line, rex_assert_body)):
          line = line.replace("$error(", "$fwrite(32'h80000002, ").replace("Assertion failed:", "")
        else:
          line = line.replace("$error(", "$fwrite(32'h80000002, ")
        assertion_queue.put(line)
        while(True):
          line = file_queue.get()
          if(match(line, rex_assert_body)):
            line = line.replace("Assertion failed:", "")
          assertion_queue.put(line)
          if(match(line, rex_assert_end)):
            ol = assertion_queue.get()
            res_queue.put(gen_prefix(ol) + "$fwrite(32'h80000002, \"Assertion failed: %m @ %t\", $time);\n")
            res_queue.put(ol)

            while(not assertion_queue.empty()):
              res_queue.put(assertion_queue.get())
            res_queue.put(gen_spaces(line) + "end\n")
            break
    else:
      res_queue.put(line)

    if(file_queue.empty()):
      break

  return res_queue

def print_queue(filename, line_queue):
  with open(filename,"w") as f:
    while(True):
      line = line_queue.get()
      f.write(line)
      if(line_queue.empty()):
        break

if __name__ == "__main__":
  parser = argparse.ArgumentParser(description='RTL Assertion Replacer for XS')
  parser.add_argument('infile', type=str, help='RTL target file')
  parser.add_argument('-o', '--output', type=str, help='RTL output file')
  args = parser.parse_args()
  curdir = os.path.abspath(os.curdir)
  file_path = os.path.join(curdir,args.infile)
  out_path = os.path.join(curdir,args.output)

  if(not os.path.exists(file_path)):
    print("Input file not exsist!")
    os._exit()

  file_queue = queue.Queue()
  with open(file_path,"r") as f:
    file_lines = f.readlines()
    for line in file_lines:
      file_queue.put(line)

  stage1_queue = alter_print_info(file_queue)
  print_queue(out_path, stage1_queue)
