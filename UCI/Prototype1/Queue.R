#QUEUE STRUCTURE
#
#
# Function to enqueue an element into the queue
enqueue <- function(queue, element) {
  return(append(queue,element))
}

# Function to dequeue an element from the queue
dequeue <- function(queue) {
  if (length(queue) == 0) {
    cat("Queue is empty. Nothing to dequeue.\n")
    return(queue)
  }
  return(queue[-1])
}

is_empty <- function(queue) {
  if (length(queue) == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

delete <- function(queue, id){
  del <- 0
  if(length(queue)==0){
    return(queue)
  }
  for(i in 1:length(queue)){
    if(queue[i] == id){
      del <- i
    }
  }
  if(del == 0){
    return(queue)
  } else if(i > 0 && length(queue) > 1){
    return(queue[-del])
  }
  return(c())
}
#
#
#END QUEUE STRUCTURE
