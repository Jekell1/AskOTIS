       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WORCMD.
      *****************************************************
      *  DIR : GB
      *  DESC: WINOTIS RECEIVE COMMAND
      *  REV :
      *    JHP 052518 CREATED.
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
           "@(#)UT/WORCMD S35 03/31/23-09:24:39 >".
       01 WS-END-TAG-IDX                PIC 9(4).

       01 WS-INPUT-LEN                  PIC 9(4).
       01 WS-INPUT.
            03 WS-INPUT-VALUE                   PIC X(1440).
            03 WS-INPUT-ARR REDEFINES WS-INPUT-VALUE
                OCCURS 1440 TIMES               PIC X.  
       77 WINOTIS-TAG                   PIC X(17) 
                            VALUE "<[(WINOTISCMMD)]>".
       01 TAG-TEST                      PIC X(17).
       01 NUMERIC-TEST                  PIC X(4).              


       LINKAGE SECTION.
       01 COMMAND-ID                    PIC 9(4).
       01 COMMAND-DATA-LEN              PIC 9(4).
       01 COMMAND-DATA                  PIC X(1400).
       01 RECEIVE-ERR                   PIC 9.

       PROCEDURE DIVISION USING
            COMMAND-ID 
            COMMAND-DATA-LEN
            COMMAND-DATA
            RECEIVE-ERR.

       MAIN-MODULE.
            ACCEPT WS-INPUT FROM CONSOLE.
      D     STOP "MSG".            
            MOVE 1434 TO WS-INPUT-LEN.
            DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.


       RECEIVE-WINOTIS-DATA-LOOP.
            IF WS-INPUT-ARR(WS-INPUT-LEN) <> " "
                GO TO RECEIVE-WINOTIS-CHECK.
            SUBTRACT 1 FROM WS-INPUT-LEN.
            GO TO RECEIVE-WINOTIS-DATA-LOOP.
       
       RECEIVE-WINOTIS-CHECK.
            IF WS-INPUT-LEN < 42
                GO TO RECEIVE-WINOTIS-ERROR.
            MOVE WS-INPUT-VALUE(1:17) TO TAG-TEST.
            IF TAG-TEST <> WINOTIS-TAG
                GO TO RECEIVE-WINOTIS-ERROR.
            MOVE WS-INPUT-VALUE(18:4) TO NUMERIC-TEST.
            IF NUMERIC-TEST IS NOT NUMERIC
                GO TO RECEIVE-WINOTIS-ERROR.
            MOVE WS-INPUT-VALUE(22:4) TO NUMERIC-TEST.
            IF NUMERIC-TEST IS NOT NUMERIC
                GO TO RECEIVE-WINOTIS-ERROR.
            MOVE WS-INPUT-VALUE(22:4) TO WS-END-TAG-IDX.
            ADD 26 TO WS-END-TAG-IDX.
            MOVE WS-INPUT-VALUE(WS-END-TAG-IDX:17) TO TAG-TEST.            
            IF TAG-TEST <> WINOTIS-TAG
                GO TO RECEIVE-WINOTIS-ERROR.
            MOVE WS-INPUT-VALUE(18:4) TO COMMAND-ID.
            MOVE WS-INPUT-VALUE(22:4) TO COMMAND-DATA-LEN.
            IF COMMAND-DATA-LEN > 0
                MOVE WS-INPUT-VALUE(26:COMMAND-DATA-LEN) TO 
                    COMMAND-DATA.
            MOVE 0 TO RECEIVE-ERR
            GO TO MAIN-MODULE-EXIT.

       RECEIVE-WINOTIS-ERROR.
            MOVE 1 TO RECEIVE-ERR.
            
       MAIN-MODULE-EXIT.
            EXIT PROGRAM.
