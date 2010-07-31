#!/usr/bin/python

"""
Python script to replace xmessage when not available.

Will use pygtk to make a box if xmessage is not available.
"""

import os, string, sys

def find_in_path(file, path=None):
  """find_in_path(file[, path=os.environ['PATH']]) -> list

  Finds all files with a specified name that exist in the operating system's
  search path (os.environ['PATH']), and returns them as a list in the same
  order as the path.  Instead of using the operating system's search path,
  the path argument can specify an alternative path, either as a list of paths
  of directories, or as a single string seperated by the character os.pathsep.

  If you want to limit the found files to those with particular properties,
  use filter() or which()."""

  if path is None:
    path = os.environ.get('PATH', '')
  if type(path) is type(''):
    path = string.split(path, os.pathsep)
  return filter(os.path.exists,
                map(lambda dir, file=file: os.path.join(dir, file), path))

def which(file, mode=os.F_OK | os.X_OK, path=None):
  """which(file[, mode][, path=os.environ['PATH']]) -> list

  Finds all executable files in the operating system's search path
  (os.environ['PATH']), and returns them as a list in the same order as the
  path.  Like the UNIX shell command 'which'.  Instead of using the operating
  system's search path, the path argument can specify an alternative path,
  either as a list of paths of directories, or as a single string seperated by
  the character os.pathsep.

  Alternatively, mode can be changed to a different os.access mode to
  check for files or directories other than "executable files".  For example,
  you can additionally enforce that the file be readable by specifying
  mode = os.F_OK | os.X_OK | os.R_OK."""

  return filter(lambda path, mode=mode: os.access(path, mode),
                find_in_path(file, path))

def show_message(msg_app):
	os.execv(msg_app, sys.argv)

if which("xmessage") != []:
	show_message(which("xmessage")[0])

import pygtk
pygtk.require('2.0')
import gtk

class MessageDisplayer:
    def delete_event(self, widget, event, data=None):
        return False

    def destroy(self, widget, data=None):
        print "destroy signal occurred"
        gtk.main_quit()

    def __init__(self, msg):
		self.msg = " ".join(msg[1:])

		self.window = gtk.Window(gtk.WINDOW_TOPLEVEL)
		self.window.connect("delete_event", self.delete_event)
		self.window.connect("destroy", self.destroy)
		self.window.set_border_width(10)
		self.button = gtk.Button(self.msg)
		self.button.connect_object("clicked", gtk.Widget.destroy, self.window)
		self.window.add(self.button)
		self.button.show()
		self.window.show()

display = MessageDisplayer(sys.argv)
gtk.main()

