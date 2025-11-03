       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WOSBLK.
      *****************************************************
      *  DIR : GB
      *  DESC: WINOTIS BLOCK DATA SEND
      *  REV :
      *    JHP 06192018 CREATED.
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/WOSBLK S35 03/31/23-09:24:39 >".
	   
       01 MSG-BUFFER-IDX                     PIC 9(4).
       01 MSG-TEMP-IDX                       PIC 9(4).
       01 MSG-TEMP.
           03 MSG-TEMP-VALUE                PIC X(1396).
           03 MSG-TEMP-ARRAY REDEFINES MSG-TEMP-VALUE
               OCCURS 1396 TIMES              PIC X.

       01 MESSAGE-BLOCKS-IDX                  PIC 9(3).
       01 MESSAGE-BLOCKS-COUNT                PIC 9(3).
       01 MESSAGE-BLOCKS OCCURS 20 TIMES.
           03 MESSAGE-BLOCK-VALUE             PIC X(1400).
           03 MESSAGE-BLOCK-RECORD 
               REDEFINES MESSAGE-BLOCK-VALUE.
               05 MESSAGE-BLOCK-ID            PIC 9(3).
               05 MESSAGE-BLOCK-LAST          PIC X.
               05 MESSAGE-BLOCK-DATA          PIC X(1396).   
           03 MESSAGE-BLOCK-LEN               PIC 9(4).         
       01 SEND-ERR-FLAG                       PIC 9. 

       LINKAGE SECTION.
       01 COMMAND-ID                           PIC 9(4).
       01 MSG-BUFFER-LEN                       PIC 9(4).
       01 MSG-BUFFER.           
           03 MSG-BUFFER-VALUE                 PIC X(16384).
           03 MSG-BUFFER-ARRAY REDEFINES MSG-BUFFER-VALUE
               OCCURS 16384 TIMES              PIC X.        
       01 ERR-FLAG                             PIC 9.


       PROCEDURE DIVISION USING
            COMMAND-ID
            MSG-BUFFER-LEN
            MSG-BUFFER
            ERR-FLAG.

       MAIN-MODULE.
           MOVE 1 TO ERR-FLAG.
           MOVE 0 TO MSG-TEMP-IDX.
           MOVE 0 TO MESSAGE-BLOCKS-COUNT.
       
       FILL-LOOP-INIT.
           MOVE SPACES TO MSG-TEMP-VALUE.
           MOVE 0 TO MSG-BUFFER-IDX.

       FILL-LOOP.
           IF MSG-BUFFER-IDX >= MSG-BUFFER-LEN
               GO TO MOVE-BLOCK-TO-BUFFER.
           IF MSG-TEMP-IDX >= 1396
               GO TO MOVE-BLOCK-TO-BUFFER.
           ADD 1 TO MSG-BUFFER-IDX.
           ADD 1 TO MSG-TEMP-IDX.
           MOVE MSG-BUFFER-ARRAY(MSG-BUFFER-IDX) TO 
               MSG-TEMP-ARRAY(MSG-TEMP-IDX).            
           GO TO FILL-LOOP.
       
       MOVE-BLOCK-TO-BUFFER.
           ADD 1 TO MESSAGE-BLOCKS-COUNT.
           MOVE SPACES TO 
               MESSAGE-BLOCK-VALUE(MESSAGE-BLOCKS-COUNT).
           MOVE MESSAGE-BLOCKS-COUNT TO 
               MESSAGE-BLOCK-ID(MESSAGE-BLOCKS-COUNT). 
           MOVE MSG-TEMP-IDX TO 
               MESSAGE-BLOCK-LEN(MESSAGE-BLOCKS-COUNT).
           ADD 4 TO MESSAGE-BLOCK-LEN(MESSAGE-BLOCKS-COUNT).
           IF MSG-TEMP-IDX > 0
               MOVE MSG-TEMP-VALUE TO 
                   MESSAGE-BLOCK-DATA(MESSAGE-BLOCKS-COUNT).
           IF MSG-TEMP-IDX < 1396
               MOVE "Y" TO 
                   MESSAGE-BLOCK-LAST(MESSAGE-BLOCKS-COUNT)
               GO TO SEND-LOOP-INIT
           ELSE
               MOVE "N"  TO 
                   MESSAGE-BLOCK-LAST(MESSAGE-BLOCKS-COUNT).
           GO TO FILL-LOOP-INIT.
                 
       SEND-LOOP-INIT.
           MOVE 1 TO MESSAGE-BLOCKS-IDX.
        
       SEND-LOOP.
           MOVE 0 TO SEND-ERR-FLAG.
           CALL "WOSCMD" USING
               COMMAND-ID
               MESSAGE-BLOCK-VALUE(MESSAGE-BLOCKS-IDX)
               MESSAGE-BLOCK-LEN(MESSAGE-BLOCKS-IDX)
               SEND-ERR-FLAG.
           CANCEL "WOSCMD".
           IF SEND-ERR-FLAG <> 0
               GO TO MAIN-MODULE-EXIT.
           IF MESSAGE-BLOCKS-IDX >= MESSAGE-BLOCKS-COUNT
               MOVE 0 TO ERR-FLAG
               GO TO MAIN-MODULE-EXIT.
           ADD 1 TO MESSAGE-BLOCKS-IDX.
           GO TO SEND-LOOP.

       MAIN-MODULE-EXIT.
           EXIT PROGRAM.



           




            

             
