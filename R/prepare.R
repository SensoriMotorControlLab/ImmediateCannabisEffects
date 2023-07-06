
renameData <- function(task='', oldtaskname='', test=FALSE) {
  
  if (nchar(task) == 0 | nchar(oldtaskname) == 0) {
    cat('please set the new and old task name\n')
    return()
  }
  
  # see if we can find a directory in the temp directory that might have csv data files for the named task
  tempdirs <- list.dirs('temp/', recursive=FALSE, full.names = FALSE)
  # check if the directory is named after the new task name:
  if (task %in% tempdirs) {
    taskdir <- sprintf('temp/%s', task)
  }
  if (sprintf('%s-master',task) %in% tempdirs) {
    taskdir <- sprintf('temp/%s-master', task)
  }
  if (!exists('taskdir')) {
    if (oldtaskname %in% tempdirs) {
      taskdir <- sprintf('temp/%s', oldtaskname)
    }
    if (sprintf('%s-master',oldtaskname) %in% tempdirs) {
      taskdir <- sprintf('temp/%s-master', oldtaskname)
    }
  }
  
  if (!exists('taskdir')) {
    cat('can not find task directory: check task name\n')
    return()
  }
  
  tempdirs <- list.dirs(taskdir, recursive=FALSE, full.names=FALSE)
  if ('data' %in% tempdirs) {
    datadir <- sprintf('%s/data', taskdir)
  } else {
    datadir <- taskdir
  }
  
  csv_files <- list.files(datadir, pattern = '*.csv')
  
  #print(length(csv_files))
  
  for (filename in csv_files) {
    
    pattern <- sprintf('_%s_', oldtaskname)
    
    pos <- gregexpr(pattern=pattern, filename)[[1]][1]
    if (pos < 0) {
      cat('can not find old task name in file name\n')
      next
    }
    pp <- substr(filename, 1, pos-1)
    ts <- substr(filename, pos+nchar(pattern), nchar(filename)-4)
    
    newfilename <- sprintf('%s_%s_%s.csv', pp, task, ts)
    
    if (test) {
      print(newfilename)
    } else {
      file.rename(from = sprintf('%s/%s', datadir, filename),
                  to   = sprintf('%s/%s', datadir, newfilename))
    }

  }
  
  
  
}