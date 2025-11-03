      * A30 CONVERSION STARTED
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLLO3.  
       AUTHOR.        RMZ.
       DATE-WRITTEN.  10-15-2024.
      ******************************************************************
      *
      * IDENTIFICATION:  WI/SQLLO3.CBL (#S35Q-94 & #S35Q-104)
      *
      * DESCRIPTION: THIS PROGRAM READS A BUFFER PASSED FROM PROGRAM 
      *              WI/SQLLO2 OF A SELECTED SQL LOG RECORD, AND SHOWS     
      *              ALL THE INFORMATION FOR THAT SQL LOG RECORD.  
      *
      *-----------------------------------------------------------------
      * REV:
      * XXX YYMMDD -    
      *
      *
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.                 COMPUTER-NAME.
       OBJECT-COMPUTER.                 COMPUTER-NAME.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
 
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/SQLLO3 S35 10/15/24-10:25:57 >".

       EXEC SQL INCLUDE SQLCA END-EXEC.
       EXEC SQL INCLUDE SQLDA END-EXEC.

      *********************************************************
      *        LIBLP/LPWSRN: RN FILE PATH
      *********************************************************
       01  WK-PATH                           PIC X(35).
       01  RN-PATH                           PIC X(35).

  
       01  WK-LINE.
           03  WK-DATE-YYYY-MM-DD            PIC 9999/99/99.
           03  FILLER                        PIC X(1).
           03  WK-TIME                       PIC 99/99/99.
           03  FILLER                        PIC X(1).
           03  WK-PROGRAM-NAME               PIC X(40).
           03  FILLER                        PIC X(1).
           03  WK-PROGRAM-RANGES             PIC X(18).
           03  FILLER                        PIC X(1).
           03  WK-STATUS-CODE                PIC X(4).
           03  FILLER                        PIC X(1).
           03  WK-DESCRIPTION                PIC X(150).
           03  FILLER                        PIC X(1).
           03  WK-DB-NAME                    PIC X(40). 

       78  LIST-BOX-LINES                    VALUE 19.
       01  SQLER2-POP-UP-WINDOW              PIC X(10).
       01  SQLER2-WINDOW-TITLE               PIC X(30).
       01  SENDMESS-POP-UP-WINDOW            PIC X(10). 
       
       COPY "LIBGB/ACUCOBOL_DEF.CPY".      
       COPY "LIBGB/ACUGUI_DEF.CPY".        
       
       77  NUMBER-READS-NEEDED               PIC 99.

      * STATE-FLAG TRACKS WHERE YOU ARE IN THE FILE.
      * READING-FORWARDS INDICATES THAT THE LAST READ WAS
      * OF THE LAST RECORD SHOWN IN THE BOX.
      * READING-BACKWARDS INDICATES THAT THE LAST READ
      * WAS OF THE FIRST RECORD IN THE BOX (VIA READ
      * PREVIOUS).  AT-START AND AT-END HANDLE SPECIAL
      * CASES WHEN YOU REACH EITHER END OF THE FILE.

       77  STATE-FLAG                        PIC X.
           88  READING-FORWARDS              VALUE "F".
           88  READING-BACKWARDS             VALUE "B".
           88  AT-START                      VALUE "S".
           88  AT-END                        VALUE "E".

       77  LIST-DATA                         PIC X(78).

      *    VI +/\"\FIELD"  /$REL/HELP/
       01  HELP-CALL-VI.
           03  FILLER                        PIC X(6)  VALUE
               X"7669202B2F22".        
           03  HELP-CALL-VI-FIELD            PIC X(77) VALUE " ".
           03  HELP-CALL-VI-FILE.      
               05  FILLER                    PIC X(11) VALUE
                   X"2F2452454C2F48454C502F".
               05  HELP-NAM-VI               PIC X(9)  VALUE " ".

      *    /$REL/SH/HELP.SH
       01  HELP-CALL-SH.
           03  FILLER                        PIC X(17) VALUE
               X"2F2452454C2F53482F68656C702E736820".
           03  HELP-NAM-SH                   PIC X(9)  VALUE " ".
         
       01  HEADER-COUNT                      PIC 9(09).
       
       01  NOTE-TYPE                         PIC X(01).
           88 NOTE-TYPE-PROGRAMMER           VALUE "P" X'70'.

       01  SCAN-LINE-BLANK.      
           03  FILLER                        PIC X(78) VALUE SPACES.

       01  SCAN-LINE1-HEAD.                      
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(10) VALUE 
               "DATE      ".
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(08) VALUE
               "TIME    ".  
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(04) VALUE
               "STAT".
           03  FILLER                        PIC X(53) VALUE SPACES.
 
      
       01  SCAN-LINE1.                      
           03  FILLER                        PIC X(01).
           03  SCAN-DATE-YYYY-MM-DD          PIC X(10).
           03  FILLER                        PIC X(01).
           03  SCAN-TIME                     PIC X(08).
           03  FILLER                        PIC X(01).
           03  SCAN-STATUS-CODE              PIC X(04).
           03  FILLER                        PIC X(53).

   
       01  SCAN-LINE2-HEAD.                 
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(18) VALUE 
               "RANGES            ". 
           03  FILLER                        PIC X(59) VALUE SPACES.


       01  SCAN-LINE2.   
           03  FILLER                        PIC X(01).
           03  SCAN-PROGRAM-RANGES           PIC X(18). 
           03  FILLER                        PIC X(59) VALUE SPACES.
  
  
       01  SCAN-LINE3-HEAD.                  
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(40) VALUE 
               "PROGRAM NAME                            ".
           03  FILLER                        PIC X(37) VALUE SPACES.
 
     
       01  SCAN-LINE3.                       
           03  FILLER                        PIC X(01).
           03  SCAN-PROGRAM-NAME             PIC X(40).
           03  FILLER                        PIC X(37) VALUE SPACES.
 
       01  SCAN-LINE4-HEAD.                  
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(40) VALUE 
               "DATABASE NAME                           ".
           03  FILLER                        PIC X(37) VALUE SPACES.
 
     
       01  SCAN-LINE4.                       
           03  FILLER                        PIC X(01).
           03  SCAN-DB-NAME                  PIC X(40).
           03  FILLER                        PIC X(37) VALUE SPACES.

     
       01  SCAN-LINE5-HEAD.                       
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(75)  VALUE 
               "DESCRIPTION ".
           03  FILLER                        PIC X(02).
    
       01  SCAN-LINE5.                       
           03  FILLER                        PIC X(01).
           03  SCAN-DESCRIPTION1             PIC X(75).  
           03  FILLER                        PIC X(02) VALUE SPACES.
     
       01  SCAN-LINE6.
           03  FILLER                        PIC X(01). 
           03  SCAN-DESCRIPTION2             PIC X(75).  
           03  FILLER                        PIC X(02) VALUE SPACES.
         


      *77  KEY-STATUS
      *    IS SPECIAL-NAMES CRT STATUS  PIC 9(4).

      * NOTE: ALTHOUGH THE PROGRAM NEVER DIRECTLY
      * REFERENCES THE SCREEN CONTROL DATA ITEM, YOU HAVE
      * TO DECLARE IT ANYWAY.  THAT IS BECAUSE "NOTIFY"
      * STYLE EVENTS WILL SET IT TO THE PROPER VALUE WHEN
      * ENTERING AN EXCEPTION PROCEDURE.  THE VALUE
      * GENERATED CAUSES THE CONTROLLING ACCEPT STATEMENT
      * TO CONTINUE PROCESSING AFTER THE EXCEPTION
      * PROCEDURE FINISHES.  IF YOU OMIT SCREEN-CONTROL,
      * THEN THE RESULT IS THAT YOU EXIT THE ACCEPT AFTER
      * THE EXCEPTION PROCEDURE COMPLETES.

       01  SCREEN-CONTROL
           IS SPECIAL-NAMES SCREEN CONTROL.
           03  ACCEPT-CONTROL            PIC 9.
           03  CONTROL-VALUE             PIC 999.

       01  EVENT-STATUS
           IS SPECIAL-NAMES EVENT STATUS.
           03  EVENT-TYPE                PIC X(4) COMP-X.
           03  EVENT-WINDOW-HANDLE       HANDLE OF WINDOW.
           03  EVENT-CONTROL-HANDLE      HANDLE.
           03  EVENT-CONTROL-ID          PIC X(2) COMP-X.
           03  EVENT-DATA-1              SIGNED-SHORT.
           03  EVENT-DATA-2              SIGNED-LONG.
      *    03  EVENT-ACTION              PIC X COMP-X.
  
       01  COLUMN-TITLE                  PIC X(25) VALUE
           "SQL LOG DETAIL     ".
       01  COLUMN-HEADINGS               PIC X(19) VALUE
           "   DIRECTORIES:    ".
       01  COLUMN-HEADINGS2.
           03  FILLER                    PIC X(39) VALUE
           "---------------------------------------".
           03  FILLER                    PIC X(39) VALUE
           "---------------------------------------".
 

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/GETFMW.CPY".

       01  SQLERR-BUF.
           03  LINK-FILE       PIC X(20).
           03  LINK-HEADER     PIC X(40). 
           03  LINK-LINE       PIC X(500).  

       SCREEN SECTION.
       01  SCREEN-1.
           03  LABEL LINE 02 COL 02 'DATE      '           ,UNDERLINE.
           03  LABEL LINE 02 COL 13 'TIME    '             ,UNDERLINE.
           03  LABEL LINE 02 COL 22 'STAT'                 ,UNDERLINE.
      *    ***                 ***
           03  LABEL LINE 03 COL 02 SCAN-DATE-YYYY-MM-DD   ,LOW.
           03  LABEL LINE 03 COL 13 SCAN-TIME              ,LOW.
           03  LABEL LINE 03 COL 22 SCAN-STATUS-CODE       ,LOW.
      *    ***                 ***
           03  LABEL LINE 04 COL 02 
               "                                      "    ,LOW.
           03  LABEL LINE 04 COL 41
               "                                      "    ,LOW.
      *    ***                 ***
           03  LABEL LINE 05 COL 02 'RANGES            '   ,UNDERLINE.
      *    ***                 ***
           03  LABEL LINE 06 COL 02 SCAN-PROGRAM-RANGES    ,LOW.
      *    ***                 ***
           03  LABEL LINE 07 COL 02 
               "                                      "    ,LOW.
           03  LABEL LINE 07 COL 41
               "                                      "    ,LOW.
      *    ***                 ***
           03  LABEL LINE 08 COL 02 'PROGRAM NAME '        ,UNDERLINE.
           03  LABEL LINE 08 COL 15 '                 '    ,UNDERLINE.
           03  LABEL LINE 08 COL 31 '           '          ,UNDERLINE.
      *    ***                 ***
           03  LABEL LINE 09 COL 02 SCAN-PROGRAM-NAME      ,LOW.
      *    ***                 ***
           03  LABEL LINE 10 COL 02 
               "                                      "    ,LOW.
           03  LABEL LINE 10 COL 41
               "                                      "    ,LOW.
      *    ***                 ***
           03  LABEL LINE 11 COL 02 'DATABASE NAME   '     ,UNDERLINE.
           03  LABEL LINE 11 COL 17 '               '      ,UNDERLINE.
           03  LABEL LINE 11 COL 31 '           '          ,UNDERLINE.
      *    ***                 ***
           03  LABEL LINE 12 COL 02 SCAN-DB-NAME           ,LOW.
      *    ***                 ***
           03  LABEL LINE 13 COL 02 
               "                                      "    ,LOW.
           03  LABEL LINE 13 COL 41
               "                                      "    ,LOW.
      *    ***                 ***
           03  LABEL LINE 14 COL 02 'DESCRIPTION'          ,UNDERLINE.
           03  LABEL LINE 14 COL 13 '                  '   ,UNDERLINE.
           03  LABEL LINE 14 COL 31 '                    ' ,UNDERLINE.
           03  LABEL LINE 14 COL 51 '                    ' ,UNDERLINE.
           03  LABEL LINE 14 COL 71 '      '               ,UNDERLINE.
      *    ***                 ***
           03  LABEL LINE 15 COL 02 SCAN-DESCRIPTION1      ,LOW. 
      *    ***                 ***
           03  LABEL LINE 16 COL 02 SCAN-DESCRIPTION2      ,LOW.
      *    ***                 ***
           03  LIST-1, LIST-BOX USING LIST-DATA,
      *        LINE 03, COLUMN 01, SIZE 78,
               LINE 17, COLUMN 01, SIZE 78,
      *        LINES LIST-BOX-LINES + VDU-WIN-H-MOD
               LINES LIST-BOX-LINES 
               PAGED, NO-BOX, NO-SEARCH, COLOR IS 8.
      *        EXCEPTION PROCEDURE IS LIST-1-HANDLER.

       01 SENDMESS-WINDOW.
          03  USING MESS
              LINE 001
              COL 001
              BOLD
              CHARACTER
              COLOR IS 8
              PROMPT " "
              NO ECHO.
          03  LINE 001
              COL 062
              "PRESS <ENTER>:"
              BOLD
              COLOR IS 8.
       01 SENDMESS-ACCEPT.
          03  USING STAT
              LINE 001
              COL 076
              BOLD
              CHARACTER
              COLOR IS 8
              PROMPT " ".


       PROCEDURE DIVISION USING FORM-PATHNAME SQLERR-BUF EXIT-PATHNAME.

      D    STOP MESS.

      ******************************************************************
       MAIN-LOGIC SECTION.
      ******************************************************************
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE LINK-HEADER   TO RN-PATH.
           MOVE LINK-LINE     TO WK-LINE.
 
           MOVE WK-DATE-YYYY-MM-DD     TO SCAN-DATE-YYYY-MM-DD.                      
           MOVE WK-TIME                TO SCAN-TIME.
           MOVE WK-PROGRAM-NAME        TO SCAN-PROGRAM-NAME. 
           MOVE WK-PROGRAM-RANGES      TO SCAN-PROGRAM-RANGES. 
           MOVE WK-STATUS-CODE         TO SCAN-STATUS-CODE. 
           MOVE WK-DESCRIPTION(1:75)   TO SCAN-DESCRIPTION1.
           MOVE WK-DESCRIPTION(76:75)  TO SCAN-DESCRIPTION2.
           MOVE WK-DB-NAME             TO SCAN-DB-NAME.                 
  
           MOVE SPACES TO NOTE-TYPE MESS.
           ACCEPT MESS FROM ENVIRONMENT "HELPSH".
           IF MESS = X'7669'
              MOVE "P" TO NOTE-TYPE
           ELSE
              MOVE SPACES TO NOTE-TYPE.

      * OPEN WINDOW

           MOVE COLUMN-TITLE TO SQLER2-WINDOW-TITLE.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE 78 + VDU-WIN-W-MOD
                   LINES 22 + VDU-WIN-H-MOD
                   TOP CENTERED TITLE IS SQLER2-WINDOW-TITLE
                   POP-UP AREA IS SQLER2-POP-UP-WINDOW. 
        
           DISPLAY SCREEN-1.

        SCAN-ACCEPT.

           ACCEPT SCREEN-1
             ON EXCEPTION
                GO TO END-ROUTINE
           END-ACCEPT.

           MOVE "WI/SQLLO3"   TO LINK-FILE.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "WI/SQLLO2"   TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATHNAME SQLERR-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           GO TO SCAN-ACCEPT.

           GO TO END-ROUTINE.

       COPY "LIBGB/DATER.CPY".

      **************************************
       FORM-RESET SECTION.
      **************************************

      **************************************
       SEND-MESS SECTION.
      **************************************
           MOVE "00" TO STAT.
           DISPLAY FLOATING GRAPHICAL WINDOW
              AT 022002
              LINES 01 + VDU-WIN-H-MOD
              SIZE 78 + VDU-WIN-W-MOD
              TOP CENTERED TITLE IS "MESSAGE"
              POP-UP AREA IS SENDMESS-POP-UP-WINDOW.

           DISPLAY SENDMESS-WINDOW.
           ACCEPT STAT
              AT 001076
              BOLD
              COLOR IS 8
              PROMPT " "
              NO ECHO.

      * CLOSE WINDOW AND EXIT PROGRAM

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SENDMESS-POP-UP-WINDOW.

           IF STAT = "~~"
              GO TO END-ROUTINE.
           MOVE "00" TO STAT.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************

       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".

      *******************************************
       FILE-IO-ERROR SECTION.
      *******************************************
       IO-ERROR.

      *******************************************
       END-ROUTINE SECTION.
      *******************************************
 
       EXIT-PROG.
           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE "WI/SQLLO2" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ALL.
           CLOSE WINDOW SQLER2-POP-UP-WINDOW.
           EXIT PROGRAM.
