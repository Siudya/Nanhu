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

def alter_print_info(file_queue):
  rex_assert_begin = re.compile("\$fwrite\(32'h80000002")
  rex_assert_body = re.compile("Assertion failed")
  rex_assert_end = re.compile("\);")
  assertion_queue = queue.Queue()
  res_queue = queue.Queue()

  while(True):
    line = file_queue.get()
    is_single_line = match(line, rex_assert_begin) and match(line, rex_assert_end)
    is_begin = match(line, rex_assert_begin)
    if(is_single_line):
      if(match(line, rex_assert_body)):
        res_queue.put(gen_prefix(line) + "$fwrite(32'h80000002, \"Assertion failed: %m @ %t\", $time);\n")
        line = line.replace("Assertion failed", "")
      res_queue.put(line)

    elif(is_begin):
      is_assert = False
      if(match(line, rex_assert_body)):
        line = line.replace("Assertion failed", "")
        is_assert = True
      assertion_queue.put(line)
      while(True):
        line = file_queue.get()
        if(match(line, rex_assert_body)):
          is_assert = True
          line = line.replace("Assertion failed", "")

        assertion_queue.put(line)
        if(match(line, rex_assert_end)):
          if(is_assert):
            ol = assertion_queue.get()
            res_queue.put(gen_prefix(ol) + "$fwrite(32'h80000002, \"Assertion failed: %m @ %t\", $time);\n")
            res_queue.put(ol)

          while(not assertion_queue.empty()):
            res_queue.put(assertion_queue.get())

          break

    else:
      res_queue.put(line)

    if(file_queue.empty()):
      break

  return res_queue

def alter_assert_always_block(line_queue):
  rex_always_statement = re.compile("always @\(posedge clock\)")
  rex_begin = re.compile("\\bbegin\\b")
  rex_end = re.compile("\\bend\\b")
  rex_assert = re.compile("^(?!.*\").*assert\(.*\);")
  always_block = queue.Queue()
  res_queue = queue.Queue()
  begin_stack = []
  line_num = 0
  meet_always_block = False
  block_has_assert = False

  while not line_queue.empty():
    line = line_queue.get()
    if not meet_always_block:
      meet_always_block = match(line, rex_always_statement)

    if(not meet_always_block):
      res_queue.put(line)
    else:
      always_block.put(line)
      if not block_has_assert:
        block_has_assert = match(line, rex_assert)

      if(match(line, rex_begin)):
        begin_stack.append(line_num)

      if(match(line, rex_end)):
        begin_stack.pop()
        if(len(begin_stack) == 0):
          meet_always_block = False

      if not meet_always_block:
        if(block_has_assert):
          res_queue.put("`ifndef SYNTHESIS\n")
          always_block.put("`endif\n")

        while not always_block.empty():
          res_queue.put(always_block.get())

        block_has_assert = False
    line_num = line_num + 1

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
  stage2_queue = alter_assert_always_block(stage1_queue)
  print_queue(out_path, stage2_queue)
