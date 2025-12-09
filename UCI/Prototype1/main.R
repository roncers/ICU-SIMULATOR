
source("C:/Users/marti/Documents/TFG/Proyecto/UCI/Prototype1/Queue.R")

#Pick up the number linked to the columns of the patient or doctor
IndexNum <- function(n,colL){
  return((n-1)*colL)
}

#Converse each unit of time to the time unit selected for the patients log
timeConverser <- function(list,colL,timeUnit){
  for(i in 1:length(list)){
    if(i%%colL != 1) {
      list[i] <- list[i]*timeUnit
    }
  }
  return(list)
}

#Given three numbers returns true if the first one is greater than the second one and smaller than the third one minus the second one
evaluateTolerance <- function(n,t,m) {
  if(n<m + t && n>m - t)
    return(TRUE)
  else
    return(FALSE)
}



#setting the random seed
set.seed(5)

simulate <- function(maxT,maxB,maxD) {

  #Unit of time
  timeUnit <- 5

  #The function that determines if the patients come
  PatientsFunction <- function() rpois(1,1/96)
  PatientsHealing <- function() rnorm(1,5)
  HealingTolerance <- 1
  PatientsDying <- function() rnorm(1,6)
  DyingTolerance <- 1.4
  maxBeds <- maxB ##
  freeBeds <- c(1:maxBeds)
  occupiedBeds <- c()

  #number of max visit time for each patient
  MaxVisitsFunction <- function() as.integer(rexp(n = 1, rate = 0.4))
  #A list that saves the number of visits asociated with each patient
  patientVisit <- c()



  #The function that determines if the doctors attend and sit the patient on a UCI bed
  DoctorsAttend <- function() rnorm(1,2)
  #The tolerance is when the doctor is attending
  DoctorsTolerance <- 0.2
  #The doctors are able to take a break, I will evaluate it with as.integer
  DoctorsBreak <- function() rexp(n = 1, rate = 1.5)
  maxDoctors <- maxD
  #Determines the total number of doctors
  doctors <- c(1:maxDoctors)
  #The doctors work 5 by 5
  turnIndex <- maxDoctors / 3
  #Determines the number of busy and free doctors in a iteration
  freeDoctors <- c()
  busyDoctors <- c()
  doctorsLog <- c()

  #Displayed data
  colLength <- 9

  #288 24h   4032
  maxTime <- maxT
  t <- 0

  finalLog <- matrix(nrow = maxTime, ncol = abs(maxTime/5))
  waitToEnter <- c()
  waitToExit <- c()
  removeCorpse <- c()
  beds <- c()

  log <- c()



  #sets some variables for the loop
  id <- 1 #id of the patients
  lastT <- 0 #last time when a patient arrived
  actualTurn <-0 #The number assigned to the actual doctors turn
  turnTime <-0 #The time a turn has been working

  oneBone <- TRUE
  doctorData <- 6
  patientDataL <- 7



  while (t<maxTime) {
    if(t!=0){
      finalLog[t+1,] <- finalLog[t,]
      finalLog[t+1,1] <- t*timeUnit
    } else {
      ac <- 0
      finalLog[t+1,1] <- t*timeUnit
      for(bed in 1:maxBeds){
        finalLog[t+1,1 + bed] <- 0
      }

    }

    #Managment of the doctor turns 480 minutes are 8 hours
    if(turnTime >= 96 || t == 0){
      for(i in 1:length(doctors)){
        if(i > actualTurn*turnIndex && i <= (actualTurn+1)*turnIndex) {
          freeDoctors <- enqueue(freeDoctors,doctors[i])
          finalLog[t+1,1+maxBeds + (i-1)*doctorData + 1] <- 1
          finalLog[t+1,1+maxBeds + (i-1)*doctorData + 2] <- t*timeUnit
          t <- t + 1
          finalLog[t+1,] <- finalLog[t,]
          finalLog[t+1,1] <- t*timeUnit
          turnTime <- turnTime + 1
        } else if(t != turnIndex && i > ((actualTurn-1)%%3)*turnIndex && i <= ((actualTurn-1)%%3 + 1)*turnIndex) {
          #the doctors exit their turn
          freeDoctors <- delete(freeDoctors, doctors[i])
          busyDoctors <- delete(busyDoctors, doctors[i]) # it could be left over

          finalLog[t+1,1+maxBeds + (i-1)*doctorData + 1] <- 0
          finalLog[t+1,1+maxBeds + (i-1)*doctorData + 2] <- t*timeUnit
          finalLog[t+1,1+maxBeds + (i-1)*doctorData + 3] <- 0
          finalLog[t+1,1+maxBeds + (i-1)*doctorData + 4] <- 0
          t <- t + 1
          finalLog[t+1,] <- finalLog[t,]
          finalLog[t+1,1] <- t*timeUnit
          turnTime <- turnTime + 1
        }
      }


      actualTurn <- (actualTurn + 1)%%3

      turnTime <- turnTime%%96
    }

    if(!is_empty(busyDoctors)){
      for(doctor in busyDoctors){
        busyDoctors <- delete(busyDoctors,doctor)
        freeDoctors <- append(freeDoctors,doctor)
        finalLog[t+1,1+maxBeds + (doctor-1)*doctorData + 3] <- 0
        finalLog[t+1,1+maxBeds + (doctor-1)*doctorData + 4] <- 0
      }
    }


    if(DoctorsBreak()>=1.2 && oneBone){
      rndm <- sample(1:turnIndex, 1)
      doctor <- freeDoctors[rndm]
      busyDoctors <- append(busyDoctors,doctor)
      freeDoctors <- delete(freeDoctors,doctor)
      finalLog[t+1,1+maxBeds + (doctor-1)*doctorData + 4] <- 1
      if(is.na(finalLog[t+1,1+maxBeds +(doctor-1)*doctorData + 6])){
        finalLog[t+1,1+maxBeds + (doctor-1)*doctorData + 6] <- 1
      } else {
        finalLog[t+1,1+maxBeds + (doctor-1)*doctorData + 6] <- finalLog[t+1,1+maxBeds +(doctor-1)*doctorData + 6] + 1
      }

      oneBone <- FALSE
    }


     if(!is_empty(waitToEnter)){
       for(patient in waitToEnter){
         aux <- maxBeds + doctorData*length(doctors) + (patient-1)*patientDataL + 2
         if(any(is.na(finalLog[t+1,1+aux]))){
           finalLog[t+1,1+aux] <- timeUnit
         } else {
           finalLog[t+1,1+aux] <- finalLog[t+1,1+aux] + timeUnit
         }
       }
     }


    if(oneBone && PatientsFunction()){   #if a patient arrives
      #adds the patient to a queue where he waits to be attended
      #the id will be the column which represents in the log
      print(id)
      print("paciente empieza a esperar")
      patientData <- c(id, t - lastT, t, 0, 0, 0, 0, 0, 0)
      lastT <- t
      log <- enqueue(log,patientData)
      waitToEnter <- enqueue(waitToEnter,id)
      #the visit times linked with each patient
      patientVisit <- append(patientVisit,MaxVisitsFunction())
      aux <- maxBeds + doctorData*length(doctors) + (id-1)*patientDataL
      finalLog[t+1,1+aux  + 3] <- 1
      id <- id + 1
      oneBone <- FALSE
      }



    if(!is_empty(beds)){
      for(patient in beds){
        aux <- maxBeds + doctorData*length(doctors) + (patient-1)*patientDataL + 1
        if(any(is.na(finalLog[t+1,1+aux]))){
          finalLog[t+1,1+aux] <- timeUnit
        } else {
          finalLog[t+1,1+aux] <- finalLog[t+1,1+aux] + timeUnit
        }
        }
      }

    #checks if the queue is empty and if not it check if there are doctors free that can attend the patient waiting and they can last some time
    if(oneBone && !is_empty(waitToEnter) && maxBeds > length(beds) && !is_empty(freeDoctors) && evaluateTolerance(DoctorsAttend(),DoctorsTolerance,1)){
      patientID <- waitToEnter[1]
      print(patientID)
      print("paciente llevado a cama")
      waitToEnter <- dequeue(waitToEnter)
      beds <- enqueue(beds,patientID)
      log[IndexNum(patientID,colLength)+4] <- t
      rp <- sample(1:length(freeDoctors), 1)
      docID <-freeDoctors[rp]
      aux <- maxBeds + doctorData*length(doctors) + (patientID-1)*patientDataL
      finalLog[t+1,1+aux  + 3] <- 0
      bedID <- min(freeBeds)
      occupiedBeds <- append(occupiedBeds, list(b=bedID,p=patientID))
      finalLog[t+1,1+(bedID-1)+1] <- 1
      finalLog[t+1,1+aux  + 4] <- bedID
      index <- which(freeBeds == bedID)[1]
      freeBeds <- freeBeds[-index]
      finalLog[t+1,1+aux  + 5] <- 1
      finalLog[t+1,1+aux  + 6] <- 0
      finalLog[t+1,1+maxBeds + (docID-1)*doctorData + 3] <- patientID
      if(is.na(finalLog[t+1,1+maxBeds +(docID-1)*doctorData + 5])){
        finalLog[t+1,1+maxBeds + (docID-1)*doctorData + 5] <- 1
      } else {
        finalLog[t+1,1+maxBeds + (docID-1)*doctorData + 5] <- finalLog[t+1,1+maxBeds +(docID-1)*doctorData + 5] + 1
      }
      oneBone <- FALSE
    }




    #if there are patinents in beds they check if they are healed or if they die
    if(oneBone && !is_empty(beds)){
      for(patientID in beds) {
        if(oneBone && !patientID %in% waitToExit && !patientID %in% removeCorpse && evaluateTolerance(PatientsHealing(),HealingTolerance,1)){
          print(patientID)
          print("paciente curado, esperando a salir")
          waitToExit <- enqueue(waitToExit,patientID)
          log[IndexNum(patientID,colLength)+5] <- t
          aux <- maxBeds + doctorData*length(doctors) + (patientID-1)*patientDataL
          finalLog[t+1,1+aux  + 5] <- 2
          oneBone <- FALSE
        } else if(oneBone && !patientID %in% waitToExit && !patientID %in% removeCorpse && evaluateTolerance(PatientsDying(),DyingTolerance,1)){
          print(patientID)
          print("paciente fallecido")
          removeCorpse <- enqueue(removeCorpse,patientID)
          log[IndexNum(patientID,colLength)+7] <- t
          aux <- maxBeds + doctorData*length(doctors) + (patientID-1)*patientDataL
          finalLog[t+1,1+aux  + 5] <- 0
          oneBone <- FALSE
        }
      }

    }
    #if there are patiens are in beds and are in good condition

    #if a patient is helaed a doctor attends it to leave the ICU
    if(oneBone && !is_empty(beds) && !is_empty(freeDoctors) && !is_empty(waitToExit) && evaluateTolerance(DoctorsAttend(),DoctorsTolerance,1)){
      patientID <- waitToExit[1]
      print(patientID)
      print("paciente sale de la UCI")
      waitToExit <- dequeue(waitToExit)
      beds <- delete(beds,patientID)
      log[IndexNum(patientID,colLength)+6] <- t
      aux <- maxBeds + doctorData*length(doctors) + (patientID-1)*patientDataL
      rp <- sample(1:length(freeDoctors), 1)
      docID <-freeDoctors[rp]
      finalLog[t+1,1+aux  + 4] <- 0
      finalLog[t+1,1+maxBeds + (docID-1)*doctorData + 3] <- patientID
      if(is.na(finalLog[t+1,1+maxBeds +(docID-1)*doctorData + 5])){
        finalLog[t+1,1+maxBeds + (docID-1)*doctorData + 5] <- 1
      } else {
        finalLog[t+1,1+maxBeds + (docID-1)*doctorData + 5] <- finalLog[t+1,1+maxBeds +(docID-1)*doctorData + 5] + 1
      }
      finalLog[t+1,1+aux  +  7] <- t*timeUnit
      index <- which(names(occupiedBeds) == "p" & occupiedBeds == patientID)
      index <- index - 1
      bedID <- occupiedBeds[index]
      bedID <- as.numeric(bedID)
      freeBeds <- append(freeBeds,bedID)
      finalLog[t+1,1+(bedID-1)+1] <- 0
      occupiedBeds <- occupiedBeds[-index]
      occupiedBeds <- occupiedBeds[-index]
      oneBone <- FALSE

    }

    #Visits
      if(!is_empty(waitToExit)){
        for(patientID in beds) {
          if(oneBone && patientVisit[patientID] > 0 && patientID %in% waitToExit){
            log[IndexNum(patientID,colLength)+9] <- log[IndexNum(patientID,colLength)+9] + 1
            patientVisit[patientID] <- patientVisit[patientID] - 1
            aux <- maxBeds + doctorData*length(doctors) + (patientID-1)*patientDataL
            finalLog[t+1,1+aux  + 6] <- finalLog[t+1,1+aux  + 6] + 1
            oneBone <- FALSE
          }
        }
      }


    #if a patient has died and need to be removed from the bed
    if(oneBone && !is_empty(beds) && !is_empty(freeDoctors) && !is_empty(removeCorpse) && evaluateTolerance(DoctorsAttend(),DoctorsTolerance,1)){
      patientID <- removeCorpse[1]
      print(patientID)
      print("cadaver removido de la UCI")
      removeCorpse <- dequeue(removeCorpse)
      posi <- which(beds == patientID)
      beds <- delete(beds,patientID)
      log[IndexNum(patientID,colLength)+8] <- t
      aux <- maxBeds + doctorData*length(doctors) + (patientID-1)*patientDataL
      rp <- sample(1:length(freeDoctors), 1)
      docID <-freeDoctors[rp]
      finalLog[t+1,1+aux  + 4] <- 0
      finalLog[t+1,1+maxBeds +(docID-1)*doctorData + 3] <- patientID
      if(is.na(finalLog[t+1,1+maxBeds +(docID-1)*doctorData + 5])){
        finalLog[t+1,1+maxBeds +(docID-1)*doctorData + 5] <- 1
      } else {
        finalLog[t+1,1+maxBeds +(docID-1)*doctorData + 5] <- finalLog[t+1,1+maxBeds +(docID-1)*doctorData + 5] + 1
      }

      finalLog[t+1,1+aux  +  8] <- t*timeUnit
      finalLog[t+1,1+(posi-1)+1] <- 0
      index <- which(names(occupiedBeds) == "p" & occupiedBeds == patientID)
      index <- index - 1
      bedID <- occupiedBeds[index]
      bedID <- as.numeric(bedID)
      freeBeds <- append(freeBeds,bedID)
      finalLog[t+1,1+(bedID-1)+1] <- 0
      occupiedBeds <- occupiedBeds[-index]
      occupiedBeds <- occupiedBeds[-index]
      oneBone <- FALSE
    }

    t <- t + 1
    turnTime <- turnTime + 1
    oneBone <- TRUE

  }

  finalLog[is.na(finalLog)] <- ""
  LOG_df <- as.data.frame(finalLog)
  colN <- c()
  colN <- append(colN,"hora de simulacion")
  for (bed in 1:maxBeds){
    colN <- append(colN,sprintf("c%d ocupada", bed))
  }
  for (doctor in doctors){
    colN <- append(colN,sprintf("d%d trabajando", doctor))
    colN <- append(colN,sprintf("d%d Hora apertura/cierre", doctor))
    colN <- append(colN,sprintf("d%d Atendiendo a", doctor))
    colN <- append(colN,sprintf("d%d descansando", doctor))
    colN <- append(colN,sprintf("d%d Atendidos", doctor))
    colN <- append(colN,sprintf("d%d descansos", doctor))
  }

  for (patient in 1:(id-1)){
    colN <- append(colN,sprintf("p%d tiempo de ingreso", patient))
    colN <- append(colN,sprintf("p%d tiempo esperado", patient))
    colN <- append(colN,sprintf("p%d esperando", patient))
    colN <- append(colN,sprintf("p%d en cama", patient))
    colN <- append(colN,sprintf("p%d estado vital", patient))
    colN <- append(colN,sprintf("p%d visitas", patient))
    colN <- append(colN,sprintf("p%d hora salida", patient))
  }
  colnames(LOG_df) <- colN
  write.csv2( LOG_df, "log.csv", row.names = FALSE)

  log <- timeConverser(log, colLength, timeUnit)
  LOG <- matrix(log, nrow = colLength)
  LOG <- t(LOG)
  colnames(LOG) <- c("ID paciente","Tiempo entre llegadas", "Hora llegada", "Hora Atencion", "Hora curacion", "Hora alta","Hora muerte","Hora salida cadaver","tiempo de visitas")#"tiempo de visitas"
  LOG_df1 <- as.data.frame(LOG)
  write.csv2( LOG_df1, "Patient_log.csv", row.names = FALSE)

  result <- c()
  # #1 - pacientes desatendidos
  # index <- 4
  # notAttend <- 0
  # while(!is.null(log[index]) && !is.na(log[index])){
  #   #print(log[index])
  #   if(log[index] == 0){
  #     notAttend <- notAttend + 1
  #   }
  #   index <- index + colLength
  # }
  # result <- append(result,notAttend)
  #
  # #2 - atendidos por doctor media
  # attended <- 0
  # #3 - descansos por doctor media
  # rests <- 0
  # aux <- maxBeds
  # for(doctor in 1:maxDoctors){
  #       if(!is.null(as.numeric(finalLog[maxTime,aux + (doctor - 1)*7 + 6])) && !is.na(as.numeric(finalLog[maxTime,aux + (doctor - 1)*7 + 6]))){
  #         attended <- attended + as.numeric(finalLog[maxTime,aux + (doctor - 1)*7 + 6])
  #       }
  #       if(!is.null(as.numeric(finalLog[maxTime,aux + (doctor - 1)*7 + 7])) && !is.na(as.numeric(finalLog[maxTime,aux + (doctor - 1)*7 + 7]))){
  #         rests <- rests + as.numeric(finalLog[maxTime,aux + (doctor - 1)*7 + 7])
  #       }
  # }
  # result <- append(result,attended/maxDoctors)
  # result <- append(result,rests/maxDoctors)
  #
  # #4 - tiempo espera medio pacientes
  # waitingAv <- 0
  # #5 - tiempo ingreso medio pacientes
  # ingressAv <- 0
  # #6 - visitas medias pacientes
  # visitAv <- 0
  # #7 - pacientes muertos
  # deathCount <- 0
  # #8 - pacientes curados
  # healCount <- 0
  # aux <- maxBeds + maxDoctors*7
  # maxPatients <- id-1
  # for(patient in 1:maxPatients){
  #   desp <- (patient-1)*8
  #   if(!is.null(as.numeric(finalLog[maxTime,aux + desp + 3])) && !is.na(as.numeric(finalLog[maxTime,aux + desp + 3]))){
  #     waitingAv <- waitingAv + as.numeric(finalLog[maxTime,aux + desp + 3])
  #   }
  #   if(!is.null(as.numeric(finalLog[maxTime,aux + desp + 2])) && !is.na(as.numeric(finalLog[maxTime,aux + desp + 2]))){
  #     ingressAv <- ingressAv + as.numeric(finalLog[maxTime,aux + desp + 2])
  #   }
  #   if(!is.null(as.numeric(finalLog[maxTime,aux + desp + 7])) && !is.na(as.numeric(finalLog[maxTime,aux + desp + 7]))){
  #     visitAv <- visitAv + as.numeric(finalLog[maxTime,aux + desp + 7])
  #   }
  #   if(!is.null(as.numeric(finalLog[maxTime,aux + desp + 6])) && !is.na(as.numeric(finalLog[maxTime,aux + desp + 6]))){
  #     # print(finalLog[maxTime,aux + desp + 6])
  #     if(as.numeric(finalLog[maxTime,aux + desp + 6]) > 1){
  #       healCount <- healCount + 1
  #     } else if (as.numeric(finalLog[maxTime,aux + desp + 6]) < 1) {
  #       deathCount <- deathCount + 1
  #     }
  #   }
  # }
  # waitingAv <- waitingAv/maxPatients
  # ingressAv <- ingressAv/maxPatients
  # visitAv <- visitAv/maxPatients
  # result <- append(result,waitingAv)
  # result <- append(result,ingressAv)
  # result <- append(result,visitAv)
  # result <- append(result,healCount)
  # result <- append(result,deathCount)

   # #total ocupacion camas
   # totalOcupacion <- 0
   # for(i in 1:maxTime){
   #   for(bed in 1:maxBeds){
   #     if(finalLog[i,bed*2]>0){
   #       totalOcupacion <- totalOcupacion + timeUnit
   #     }
   #   }
   # }
   # result <- append(result, totalOcupacion/maxBeds)
  maxPatients <- id-1
  atendidos <- 0
  aux <- maxBeds + maxDoctors*7
  for(patient in 1:maxPatients){
    desp <- (patient-1)*8
    if(!is.null(as.numeric(finalLog[maxTime,aux + desp + 6])) && !is.na(as.numeric(finalLog[maxTime,aux + desp + 6])) && as.numeric(finalLog[maxTime,aux + desp + 6]) >= 0){
           atendidos <- atendidos + 1
         }
  }
  result <- append(result, atendidos)
  return(result)

}
getOcupationMean <- function() {
  iter <- 100
  means <- matrix(nrow = 6, ncol = 10)
  patientss <- c(3,6,9,12,15,18)
  for (patient in patientss) {
    mean <- c(0,0,0,0,0,0,0,0,0,0)
    for(cama in 1:10) {
      for (i in 1:iter) {
        print("____________________")
        print(i)
        print(cama)
        print(patient)
        print("____________________")
        set.seed(i)
        mean[cama] <- mean[cama] + simulate(4032,cama,patient)[1]
      }
      mean[cama] <- mean[cama]/iter
      }
    means[patient/3,] <- mean
    }
  print("la media total es")
  print(means)
}

getWaitingAv <- function() {
  iter <- 100
  means <- c()

  for (cama in 1:10) {

    register <- 0
    mean <- 0
    for (i in 1:iter) {
      print("____________________")
      print(i)
      print(cama)
      print(patient)
      print("____________________")
      set.seed(i)
      register <- register + simulate(4032,cama,patient)[1]
    }
    mean <- register/iter
    means <- append(means, mean)
    }
  print("la media total es es")
  print(means)
}

getMeans <- function() {
  iter <- 1:100
  means <- matrix(nrow = 10, ncol = 8)
   for (cama in 1:10) {
      mean <- c(0,0,0,0,0,0,0,0)
      for (i in iter) {
        print("____________________")
        print(i)
        print(cama)
        # print(doctor)
        print("____________________")
        set.seed(i)
        mean <- mean + simulate(4032,cama,9)
      }
      mean <- mean/100
      # for (i in 1:8){
        means[cama,] <- mean
      # }
    }

  print("la media total es es")
  print(means)
}
#getMean()
time <- 4032
#print(simulate(time,1,9))
#getOcupationMean()
simulate(4032,10,9)
#getMeans()
#simulate(4032,10,9)
#data <- read.csv('Patient_log.csv')

#print(head(data))
