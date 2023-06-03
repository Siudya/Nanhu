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
  
  if(not os.path.exists(out_path)):
    print("Output file not exsist!")
    os._exit()
    

  rex_assert_begin = re.compile("\$fwrite\(32'h80000002")
  rex_assert_body = re.compile("Assertion faild:")
  rex_assert_end = re.compile("\);")
  assertion_queue = queue.Queue()
  file_queue = queue.Queue()
  file_len = 0

  with open(file_path,"r") as f:
    file_lines = f.readlines()
    file_len = len(file_lines)
    for line in file_lines:
      file_queue.put(line)
      
  idx = 0
  with open(out_path,"w") as f:
    while(True):
      line = file_queue.get()
      is_single_line = match(line, rex_assert_begin) and match(line, rex_assert_end)
      is_begin = match(line, rex_assert_begin)
      if(is_single_line):
        if(match(line, rex_assert_body)):
          f.write(gen_prefix(line) + "$fwrite(32'h80000002, \"Assertion faild: %m @ %t\", $time);\n")
          line = line.replace("Assertion faild:", "")

        f.write(line)

      elif(is_begin):
        is_assert = False
        if(match(line, rex_assert_body)):
          line = line.replace("Assertion faild:", "")
          is_assert = True
        assertion_queue.put(line)
        while(True):
          line = file_queue.get()
          if(match(line, rex_assert_body)):
            is_assert = True
            line = line.replace("Assertion faild:", "")

          assertion_queue.put(line)
          if(match(line, rex_assert_end)):
            if(is_assert):
              ol = assertion_queue.get()
              f.write(gen_prefix(ol) + "$fwrite(32'h80000002, \"Assertion faild: %m @ %t\", $time);\n")
              f.write(ol)

            while(not assertion_queue.empty()):
              f.write(assertion_queue.get())

            break

      else:
        f.write(line)

      if(file_queue.empty()):
        break