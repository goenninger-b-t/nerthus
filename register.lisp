(pushnew (cl-fad:pathname-directory-pathname *load-truename*) asdf:*central-registry* :test 'eq)
