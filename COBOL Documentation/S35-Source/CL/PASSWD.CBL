       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PASSWD.
       DATE-WRITTEN.    122391.
      ********************************************************************
      *                   (CL)
      *          PASSWORD ENTRY CALL PROGRAM
      *
      *          IMPORTANT!!  WHEN I ADDED THE OPTION TO FORCE
      *          A PASSWORD BEING ON FILE, I JUST COPIED THE
      *          EXISTING ROUTINE TO A SEPARATE ROUTINE (TO
      *          KEEP IT CLEAN & NOT MESS ANYTHING UP).  BUT
      *          THIS MAY MEAN IF YOU ARE DOING CHANGES YOU
      *          MIGHT HAVE TO DO THEM TO BOTH ROUTINES.  BLV
      *
      * REV:
      *  012494 JTG CHANGED TO ALLOW 10 COMMON PASSWORDS
      *  990928 MG  FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  071900 BAH PASSWORD PROTECT DELETING SYSTEM GENERATED
      *             MEMOS, TFC PR# 83, WIBMEMO NEEDED THIS TEST
      *  091100 BAH HAD TO CHANGE ABOVE SEQ FOR TFC PR# 83 FROM 2 TO 3
      *  010614 BLV DIDN'T CHANGE PROGRAM, BUT ADDED & TESTED LOGIC TO
      *             FORCE EXISTANCE OF A PASSWORD IN THE FILE TO GET A
      *             GOOD STATUS.  FOR EXAMPLE, A NEW FEATURE IS ADDED THAT
      *             SOME CUSTOMERS WOULDN'T WANT TO ALLOW; THIS COULD BE
      *             USED TO SAY THAT A PASSWORD MUST EXIST FOR THE
      *             FEATURE TO BE ACCESSED.  THE IDEA IS THAT IF THERE IS
      *             A PASSWORD ASSOCIATED WITH THE FEATURE, THERE'S NO
      *             EXTRA OVERHEAD AND WE DON'T HAVE TO MESS WITH ANOTHER
      *             GLOBAL FLAG.  TO USE, THERE ARE 2 SECTIONS IN THIS
      *             PROGRAM TO UNSTAR, AND CALLING PROGRAM DOES THE SAME
      *             LOGIC AS VERIFY-PASSWORD EXCEPT TO MOVE "EX" TO
      *             PASSWDW-STAT BEFORE CALLING THIS PROGRAM.  WE DECIDED
      *             NOT TO USE THIS NOW JUST BECAUSE IT SEEMED SORT OF
      *             CONFUSING TO SUDDENLY SAY THERE HAD TO BE A PASSWORD
      *             FOR SOMETHING TO WORK, BUT THERE MAY BE FUTURE USES.
      *  031112 BLV UNSTAR LOGIC EXPLAINED ABOVE, SO NOW YOU MAY
      *             FORCE EXISTANCE OF A PASSWORD FOR A GIVEN FUNCTION.
      *             TO DO THIS, JUST MOVE "EX" TO PASSWDW-STAT BEFORE
      *             CALLING THIS PROGRAM.  FOR EXAMPLE, WE DECIDE TO
      *             ALLOW SOMETHING WE DIDN'T USED TO ALLOW, BUT WANT
      *             TO PROTECT THE FEATURE WITH A PASSWORD
      *  071022 BLF ADD 2 NEW FLAGS.  IF PW-ASK-FG IS "N" AND USER WOULD
      *             BE REQUIRED TO ENTER A FLAG BASED ON LOGIN LETTER,
      *             DON'T EVEN OPEN WINDOW, RETURN BAD STAT
      *  130528 BLM CONVERT PASSWORD TO UPPER CASE BEFORE CALLING
      *             ENCRYPTION ROUTINE, PASSWORD WILL NOT BE CASE-
      *             SENSITIVE
      * KEC 2015.1130 CHANGED TO NOT USE PE-DIR, PW-DIR & PASSWDW-DIR
      * BAH 181023 GLOBAL PEFILE
      * BAH 190918 REMOVED ACCESS-CALL ON PEFILE
      ******************************************************************
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)CL/PASSWD.CBL S34 10/21/21-10:04:19 >".
       COPY "LIBGB/GB01PW.CPY".
       COPY "LIBGB/GB01PW_SQL.CPY".
       COPY "LIBGB/GBWSPW.CPY".
       COPY "LIBGB/GB01PE.CPY".
       COPY "LIBGB/GB01PE_SQL.CPY".
       COPY "LIBGB/GBWSPE.CPY".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       01  ERRCD               PIC X VALUE " ".

       01  ALP-WK.
           03  NUM-WK          PIC 9.
       01  USER-PROFILE        PIC X.
       01  LOGNAME-BUF         PIC X(15) VALUE SPACES.

       01  PASSWORD-KEY        PIC X(14).
       01  PASSWORD-PROMPT1    PIC X(31)  VALUE " ".
       01  PASSWORD-PROMPT2    PIC X(31)  VALUE " ".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".

      ******************************************************

       COPY "LIBGB/UNAMEW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/PASSWD_DEF.CPY".
       COPY "LIBWI/PASSWD_WKS.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/PASSARGS.CPY".

      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBGB/PASSWDW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/PASSWD_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME PASSWDW-BUF EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.

       MAIN-MODULE.

      * MUST EXIST AND ENTER PASSWD FLAG MUST BE 'Y' TO PASS
           IF PASSWDW-STAT = "EN"
              PERFORM PASSWORD-MUST-EXIST-AND-FLAG-ON
              GO TO END-ROUTINE.

      * MUST EXIST LOGIC:
           IF PASSWDW-STAT = "EX"
              PERFORM PASSWORD-MUST-EXIST
              GO TO END-ROUTINE.

           PERFORM VERIFY-PASSWORD.
           GO TO END-ROUTINE.

      ***************************************************
      *   ENTER PASSWORD IF MODIFYING/ADDING/DELETING
      ***************************************************
       VERIFY-PASSWORD SECTION.
           MOVE "OK" TO PASSWDW-STAT.
           MOVE PASSWDW-PGM TO PW-PGM.
           MOVE PASSWDW-SEQ TO PW-SEQ.
           PERFORM GET-PW.
           IF IO-GOOD
              IF PW-PASSYN = "N"
      * TFC PR# 83 NEED THIS CODE BECAUSE:
      *     WIBMEMO PERFORMS PASSWD AND THEN TESTS "OK", CANT BE "OK"
      *     IF YOU HAVE IT IN THE FILE AND TURNED OFF, YOU CANT BE ALLOWED
      *     TO DELETE THE SYSTEM MEMOS
                 IF ( PASSWDW-PGM = "DELAUD" ) AND
                    ( PASSWDW-SEQ = 3        )
                    MOVE "NA" TO PASSWDW-STAT
                 END-IF
                 GO TO EXIT-VERIFY-PASSWORD
              ELSE
                 IF PW-PASSYN = "Y"
                    PERFORM VERIFY-PASSWORD-CALL
                    GO TO EXIT-VERIFY-PASSWORD
                 ELSE
                    NEXT SENTENCE
           ELSE
      * TFC PR# 83 NEED THIS CODE BECAUSE:
      *     WIBMEMO PERFORMS PASSWD AND THEN TESTS "OK", CANT BE "OK"
      *     IF YOU DONT ADD IT TO THE PWFILE, YOU CANT BE ALLOWED TO
      *     DELETE THE SYSTEM MEMOS
              IF ( PASSWDW-PGM = "DELAUD" ) AND
                 ( PASSWDW-SEQ = 3        )
                 MOVE "NA" TO PASSWDW-STAT
              END-IF
              GO TO EXIT-VERIFY-PASSWORD.

           MOVE "COMMON" TO PW-PGM.
           MOVE PW-PASSYN TO ALP-WK.
           MOVE NUM-WK TO PW-SEQ.
           PERFORM GET-PW.
           IF IO-GOOD
              IF PW-PASSYN = "Y"
                 PERFORM VERIFY-PASSWORD-CALL.

       EXIT-VERIFY-PASSWORD.
           EXIT.

      **************************************
       VERIFY-PASSWORD-CALL SECTION.
      **************************************
            IF PW-USER-PROFILE NOT = " "
               PERFORM UNAME-CALL
               MOVE UNAME-BUF TO USER-PROFILE
               IF USER-PROFILE NOT > PW-USER-PROFILE
                  GO TO VERIFY-PASSWORD-CALL-EXIT.

           IF PW-EXCEPTION-FG NOT = "Y"
              GO TO CHECK-ASK-FLAG.

           PERFORM OPEN-PE1-FILE.
           MOVE PW1-KEY TO PE-PW-KEY.
           ACCEPT LOGNAME-BUF FROM ENVIRONMENT "LOGNAME".
           MOVE LOGNAME-BUF TO PE-LOGIN.
           PERFORM READ-PE1-FILE.
           PERFORM CLOSE-PE1-FILE.
           IF IO-GOOD
              GO TO VERIFY-PASSWORD-CALL-EXIT.

       CHECK-ASK-FLAG.
           IF (PW-ASK-FG = "N" OR "Q")
               PERFORM UNAME-CALL
               MOVE UNAME-BUF TO USER-PROFILE
               IF USER-PROFILE > PW-USER-PROFILE
                  IF PW-ASK-FG = "N"
                     MOVE "OPTION: &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"
                                                          TO MESS
                     INSPECT MESS REPLACING FIRST
                     "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&" BY PW-DESC
                     INSPECT MESS REPLACING FIRST "             " BY
                                                  " NOT ALLOWED!"
                     PERFORM SEND-MESS
                  END-IF
                  MOVE "NA" TO PASSWDW-STAT
                  GO TO VERIFY-PASSWORD-CALL-EXIT.

       ENTER-PASSWORD.
           MOVE PW-PASSWORD     TO PASSWORD-KEY.
           MOVE PASSWDW-PROMPT1 TO PASSWORD-PROMPT1.
           MOVE PASSWDW-PROMPT2 TO PASSWORD-PROMPT2.
           MOVE 4           TO WINDOW-LINES.
           MOVE 35          TO WINDOW-COLUMNS.
           MOVE 10          TO WINDOW-BEGIN-Y.
           MOVE 26          TO WINDOW-BEGIN-X.
           MOVE "WI/PASSWD" TO WINDOW-FORM-NAM.

           MOVE "PASSWD" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           DISPLAY PASSWD-SCREEN.

           MOVE "S  A000PWPROMPT1." TO VDU.
           MOVE PASSWDW-PROMPT1 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000PWPROMPT2." TO VDU.
           MOVE PASSWDW-PROMPT2 TO VDUBUF.
           PERFORM SCREENIO.

       PASSWORD-ENTRY.
           MOVE "F7-EXIT, ENTER PASSWD" TO MESS.
           MOVE "E  A080PW:R." TO VDU.
           PERFORM SCREENIO.
      *    IF VDUSTATUS = "1>"
      *       IF PD-AD-D-INS-COVR NOT = 0
      *          GO TO EXIT-ADD-PREMIUM-WINDOW
      *       ELSE
      *          GO TO PASSWORD-ENTRY.
           IF VDUSTATUS NOT = "00" GO TO PASSWORD-ENTRY.

           MOVE SPACES TO PASSARGS.
           MOVE VDUBUF TO PA-PP.
           CALL "C$TOUPPER" USING PA-PP, VALUE 10.
           MOVE PW-PASSWORD TO PA-ENCRYPTED.
           PERFORM PASSWORD-CALL.
           IF PA-STAT NOT = "00"
              MOVE "S  A000WSEL:R" TO VDU
              MOVE "              WRONG!" TO VDUBUF
              PERFORM SCREENIO
              CALL "C$SLEEP" USING 1
              MOVE " " TO PASSWDW-STAT.

       EXIT-PASSWORD-ENTRY.
           DESTROY PASSWD-SCREEN.
           PERFORM CLOSE-WINDOW.
       VERIFY-PASSWORD-CALL-EXIT.
           EXIT.

      ***************************************************
      *   MUST EXIST LOGIC:
      *   VERIFY PASSWORD IS IN FILE
      ***************************************************
       PASSWORD-MUST-EXIST SECTION.
           MOVE PASSWDW-PGM TO PW-PGM.
           MOVE PASSWDW-SEQ TO PW-SEQ.
           PERFORM GET-PW.
           IF IO-BAD
              MOVE "NA" TO PASSWDW-STAT
              GO TO EXIT-MUST-EXIST.

           MOVE "OK" TO PASSWDW-STAT.
           IF IO-GOOD
              IF PW-PASSYN = "N"
                 GO TO EXIT-MUST-EXIST
              ELSE
                 IF PW-PASSYN = "Y"
                    PERFORM VERIFY-PASSWORD-CALL
                    GO TO EXIT-MUST-EXIST
                 ELSE
                    MOVE "COMMON" TO PW-PGM
                    MOVE PW-PASSYN TO ALP-WK
                    MOVE NUM-WK TO PW-SEQ
                    PERFORM GET-PW
                    IF IO-GOOD
                       IF PW-PASSYN = "Y"
                          PERFORM VERIFY-PASSWORD-CALL.
       EXIT-MUST-EXIST.
           EXIT.

      ***************************************************
      *   MUST EXIST AND THE ASK PASSWORD MUST BE ON
      ***************************************************
       PASSWORD-MUST-EXIST-AND-FLAG-ON SECTION.
           MOVE PASSWDW-PGM TO PW-PGM.
           MOVE PASSWDW-SEQ TO PW-SEQ.
           PERFORM GET-PW.
           IF IO-BAD
              MOVE "NA" TO PASSWDW-STAT
              GO TO EXIT-MUST-EXIST-AND-FLAG-ON.

           MOVE "OK" TO PASSWDW-STAT.
           IF IO-GOOD
              IF PW-PASSYN = "N"
                 MOVE "NA" TO PASSWDW-STAT
                 GO TO EXIT-MUST-EXIST-AND-FLAG-ON
              ELSE
                 IF PW-PASSYN = "Y"
                    PERFORM VERIFY-PASSWORD-CALL
                    GO TO EXIT-MUST-EXIST-AND-FLAG-ON
                 ELSE
                    MOVE "COMMON" TO PW-PGM
                    MOVE PW-PASSYN TO ALP-WK
                    MOVE NUM-WK TO PW-SEQ
                    PERFORM GET-PW
                    IF IO-GOOD
                       IF PW-PASSYN = "Y"
                          PERFORM VERIFY-PASSWORD-CALL.

       EXIT-MUST-EXIST-AND-FLAG-ON.
           EXIT.

      **************************************
       PASSWORD-CALL SECTION.
      **************************************
           MOVE "GB/ENCRYP"                  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSARGS.
           CANCEL FORM-PROGX.

      **************************************
       SETUP-LINK-INFO SECTION.
      **************************************
           MOVE PASSWD-HELP--FILE TO HELP-LINK-FILE.
           MOVE PASSWD-HELP--DIR TO HELP-LINK-DIR.

      **************************************
       FORM-RESET SECTION.
      **************************************
       VDU-CONVERT-FIELD SECTION.
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/PASSWD_EVA.CPY".
              WHEN OTHER MOVE "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/DATER.CPY".

      *************************************
       MISCELLANEOUS-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/UNAME.CPY".
       COPY "LIBGB/ACCESS.CPY".
       GET-PW.
           PERFORM OPEN-PW1-FILE.
           PERFORM READ-PW1-FILE.
           PERFORM CLOSE-PW1-FILE.

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-PW1-FILE.
           PERFORM CLOSE-PE1-FILE.
           PERFORM CLOSE-PW1-FILE.
           PERFORM CLOSE-PE1-FILE.
       COPY "LIBGB/GBPWGS_SQL.CPY".
       COPY "LIBGB/GBPEGS_SQL.CPY".
       COPY "LIBGB/GBPW1RN.CPY".
       COPY "LIBGB/GBPE1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
