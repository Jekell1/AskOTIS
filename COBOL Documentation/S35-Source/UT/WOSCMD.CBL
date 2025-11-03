       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WOSCMD.
      *****************************************************
      *  DIR : GB
      *  DESC: WINOTIS SEND COMMAND
      *  REV :
      *    JHP 052418 CREATED.
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
           "@(#)UT/WOSCMD S35 03/31/23-09:24:39 >".
	   
	   COPY "LIBUT/WINOTISCORE.CPY".
	   
       01 TCLP-WINDOW-HANDLES                  EXTERNAL.
          03 TCLP-MAIN-WINDOW-HANDLE           PIC X(10).
       
       01 COMMAND-FIELDS.
            03 COMMAND-DATA-ID          PIC X(5).
            03 COMMAND-DATA-LEN         PIC X(4).
            03 COMMAND-FIELD-DEFS.
                05 COMMAND-FIELD-01         PIC X(70).
                05 COMMAND-FIELD-02         PIC X(70).
                05 COMMAND-FIELD-03         PIC X(70).
                05 COMMAND-FIELD-04         PIC X(70).
                05 COMMAND-FIELD-05         PIC X(70).
                05 COMMAND-FIELD-06         PIC X(70).
                05 COMMAND-FIELD-07         PIC X(70).
                05 COMMAND-FIELD-08         PIC X(70).
                05 COMMAND-FIELD-09         PIC X(70).
                05 COMMAND-FIELD-10         PIC X(70).
                05 COMMAND-FIELD-11         PIC X(70).
                05 COMMAND-FIELD-12         PIC X(70).
                05 COMMAND-FIELD-13         PIC X(70).
                05 COMMAND-FIELD-14         PIC X(70).
                05 COMMAND-FIELD-15         PIC X(70).
                05 COMMAND-FIELD-16         PIC X(70).
                05 COMMAND-FIELD-17         PIC X(70).
                05 COMMAND-FIELD-18         PIC X(70).
                05 COMMAND-FIELD-19         PIC X(70).
                05 COMMAND-FIELD-20         PIC X(70).    
            03 COMMAND-FIELD-ARR REDEFINES COMMAND-FIELD-DEFS
                    OCCURS 20 TIMES         PIC X(70).

       01 COMMAND-STRING-IDX            PIC 9(4).
       01 COPY-LEN                      PIC 99.
       01 COMMAND-FIELD-IDX             PIC 99.
        01 SEND-ID.
            03 SEND-ID-DATE             PIC 9(6).
            03 SEND-ID-TIME             PIC 9(8).

       01 ACKNOWLEDGE-VALUE             PIC X.


       LINKAGE SECTION.
       01 COMMAND-ID                    PIC 9(4).
       01 COMMAND-STRING                PIC X(1400).
       01 COMMAND-STRING-LEN            PIC 9(4).
       01 ERR-FLAG                      PIC 9.

       SCREEN SECTION.
       01 COMMAND-SCREEN.
            03 LABEL, LINE 01 COLUMN 01
                VALUE "<[(WINOTISRESP)]>".
            03 ENTRY-FIELD, USING COMMAND-ID
                    LINE 01 COLUMN 30
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING COMMAND-DATA-LEN
                    LINE 01 COLUMN 40
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING SEND-ID
                    LINE 01 COLUMN 50
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING COMMAND-FIELD-01
                    LINE 02 COLUMN 01
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING COMMAND-FIELD-02
                    LINE 03 COLUMN 01
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING COMMAND-FIELD-03
                    LINE 04 COLUMN 01
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING COMMAND-FIELD-04
                    LINE 05 COLUMN 01
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING COMMAND-FIELD-05
                    LINE 06 COLUMN 01
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING COMMAND-FIELD-06
                    LINE 07 COLUMN 01
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING COMMAND-FIELD-07
                    LINE 08 COLUMN 01
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING COMMAND-FIELD-08
                    LINE 09 COLUMN 01
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING COMMAND-FIELD-09
                    LINE 10 COLUMN 01
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING COMMAND-FIELD-10
                    LINE 11 COLUMN 01
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING COMMAND-FIELD-11
                    LINE 12 COLUMN 01
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING COMMAND-FIELD-12
                    LINE 13 COLUMN 01
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING COMMAND-FIELD-13
                    LINE 14 COLUMN 01
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING COMMAND-FIELD-14
                    LINE 15 COLUMN 01
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING COMMAND-FIELD-15
                    LINE 16 COLUMN 01
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING COMMAND-FIELD-16
                    LINE 17 COLUMN 01
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING COMMAND-FIELD-17
                    LINE 18 COLUMN 01
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING COMMAND-FIELD-18
                    LINE 19 COLUMN 01
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING COMMAND-FIELD-19
                    LINE 20 COLUMN 01
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 ENTRY-FIELD, USING COMMAND-FIELD-20
                    LINE 21 COLUMN 01
                    CHARACTER
                    LEFT
                    NO-BOX.
            03 LABEL, LINE 22 COLUMN 01
                VALUE "<[(WINOTISRESP)]>".
            03 ENTRY-FIELD, USING ACKNOWLEDGE-VALUE
                    LINE 22 COLUMN 70
                    CHARACTER
                    LEFT
                    NO-BOX.
        
       PROCEDURE DIVISION USING
            COMMAND-ID 
            COMMAND-STRING
            COMMAND-STRING-LEN
            ERR-FLAG.

       MAIN-MODULE.       
            ACCEPT SEND-ID-DATE FROM DATE.
            ACCEPT SEND-ID-TIME FROM TIME.
            ACCEPT SEND-ID-TIME FROM TIME.
            MOVE SPACES TO COMMAND-FIELDS.
            MOVE COMMAND-ID TO COMMAND-DATA-ID.
            MOVE COMMAND-STRING-LEN TO COMMAND-DATA-LEN.
            MOVE 1 TO COMMAND-STRING-IDX.
            MOVE 1 TO COMMAND-FIELD-IDX.

       COMMAND-FIELD-LOOP.
            IF COMMAND-STRING-IDX >= COMMAND-STRING-LEN
                GO TO SEND-MESSAGE.
            IF COMMAND-FIELD-IDX > 20
                GO TO SEND-MESSAGE.
            IF COMMAND-STRING-IDX + 70 > COMMAND-STRING-LEN
                COMPUTE COPY-LEN = (COMMAND-STRING-LEN - 
                    COMMAND-STRING-IDX) + 1
            ELSE
                MOVE 70 TO COPY-LEN.
            MOVE COMMAND-STRING(COMMAND-STRING-IDX:COPY-LEN) TO
                COMMAND-FIELD-ARR(COMMAND-FIELD-IDX).
            ADD COPY-LEN TO COMMAND-STRING-IDX.
            ADD 1 TO COMMAND-FIELD-IDX.
            GO TO COMMAND-FIELD-LOOP.

       SEND-MESSAGE.
            DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
            DESTROY COMMAND-SCREEN.
            DISPLAY COMMAND-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
            ACCEPT ACKNOWLEDGE-VALUE.
      D     STOP "MESSAGE".
            DESTROY COMMAND-SCREEN.
            MOVE 0 TO ERR-FLAG.
       
       MAIN-MODULE-EXIT.
            EXIT PROGRAM.
