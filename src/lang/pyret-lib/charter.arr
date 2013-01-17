#lang pyret

# Charter: a program for extracting documentation from Pyret code.

import "file.arr" as file
import "directory.arr" as directory

fun extract(input-path :: String, output-path :: String):
  """extract takes as input a path to a .arr file, and
     the path to an file where it will write the formatted
     documentation (just html for now)"""
  def out: ""
	def f: file.file(input-path)
  def reading-doc: false
  fun process-lines(): # should be file.File
    def l: f.read-line()
    cond:
      | l.equals("") => ""
      | true =>
        cond:
          | l.starts-with("#--|") =>
						reading-doc = true
						# add some space
						out.append("\n\n")
						# trim off indicator
						l = l.from(3)
				  | reading-doc.and(l.starts-with("#").not()) =>
						reading-doc = false
          | true => false
        end
        cond:
          | reading-doc =>
            out.append(l.from(1))
					  out.append("\n")
					| true => false
				end
				process-lines()
    end
  end
  process-lines()
	file(output-path).write-file(out)
end

