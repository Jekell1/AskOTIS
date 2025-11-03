       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ICMAIN.
      ********************************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   INSURANCE COMPANY FILE MAINTENANCE
      *
      * REV:
      *  JTG 081188 ADDED CHANGES FOR COMMISSION TBL
      *  MJD 062195 ADDED 5 MORE INSURANCES (IC-COMMRATE 9-13)
      *  JTG 111296 CHANGED LN-INSTBL (LEVEL'S 9 & 10) TO FILLER
      *  JTG 112196 MADE IC-COMM(7) PP2
      *  MJD 991001 ADDED OPTION TO USE COMMISSION AS A PERCENTAGE OR AMOUNT.
      *             (TFC PR# 41)   FIXED BUG IN SCAN.
      *  MJD 991018 FIXED BUG, DID NOT ALLOW DOLLAR AMOUNT > 100.
      *             CHANGED TO ALLOW $ > 100 BUT EXCLUDE % > 100.
      * KEC 2018-0919 LOCAL-TO-GLOBAL
      *  CS 191125 FIXED SCAN, CAN ONLY SEE YOUR BRANCH
      ********************************************************************
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
           "@(#)MN/ICMAIN.CBL S35 02/21/24-12:35:50 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01IC.CPY".
       COPY "LIBLP/LP01IC_SQL.CPY".
       COPY "LIBLP/LPWSIC.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  ORIGINAL-IC-REC    PIC X(IC-SIZE).
       01  CHANGED-IC-REC     PIC X(IC-SIZE).

       01  HOLD-IC1-KEY       PIC 9(06).
       01  FILLER             REDEFINES HOLD-IC1-KEY.
           03  HOLD-IC-BRNO   PIC 9(04).   
           03  HOLD-IC-CODE   PIC 9(02).

       01  ERRCD           PIC X       VALUE SPACES.

       01  SUB             PIC 999     VALUE 0.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-SCAN      PIC 9       VALUE 0.
       01  COMM-TYPE       PIC X(01).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC X(20).
           03  F02         PIC X(25).
           03  F03         PIC X(25).
           03  F04         PIC X(15).
           03  F05         PIC XX.
           03  F06         PIC X(10).
           03  F07         PIC X(12).
           03  F08         PIC ZZ9.99.
           03  F081        PIC X(01).
           03  F09         PIC ZZ9.99.
           03  F091        PIC X(01).
           03  F10         PIC ZZ9.99.
           03  F101        PIC X(01).
           03  F11         PIC ZZ9.99.
           03  F111        PIC X(01).
           03  F12         PIC ZZ9.99.
           03  F121        PIC X(01).
           03  F13         PIC ZZ9.99.
           03  F131        PIC X(01).
           03  F14         PIC ZZ9.99.
           03  F141        PIC X(01).
           03  F15         PIC ZZ9.99.
           03  F151        PIC X(01).
           03  F16         PIC ZZ9.99.
           03  F161        PIC X(01).
           03  F17         PIC ZZ9.99.
           03  F171        PIC X(01).
           03  F18         PIC ZZ9.99.
           03  F181        PIC X(01).
           03  F19         PIC ZZ9.99.
           03  F191        PIC X(01).
       01  DISPLAY-LIST.
           03  FILLER      PIC X(28) VALUE
           "010 020 030 040 050 060 070 ".
           03  FILLER      PIC X(32) VALUE
           "080 081 090 091 100 101 110 111 ".
           03  FILLER      PIC X(32) VALUE
           "120 121 130 131 140 141 150 151 ".
           03  FILLER      PIC X(32) VALUE
           "160 161 170 171 180 181 190 191.".
      *    **********************************
      *    *      ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX             PIC 99   VALUE 19.
       01  SPEC-TABLE.
      * 1. COMPANY NAME
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      * 2. ADDRESS 1
           03  FILLER      PIC X(8) VALUE "E  A250 ".
      * 3. ADDRESS 2
           03  FILLER      PIC X(8) VALUE "E  A250 ".
      * 4. CITY
           03  FILLER      PIC X(8) VALUE "E  A150 ".
      * 5. STATE
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 6. ZIP CODE
           03  FILLER      PIC X(8) VALUE "E  A100 ".
      * 7. PHONE NUMBER
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      * 8. CL DEC SINGLE COMMISSION RATE                (1)
           03  FILLER      PIC X(8) VALUE "ENNN032 ".
      * 9. CL DEC JOINT COMMISSION RATE                 (2)
           03  FILLER      PIC X(8) VALUE "ENNN032 ".
      *10. CL WHOLE SINGLE COMMISSION RATE              (3)
           03  FILLER      PIC X(8) VALUE "ENNN032 ".
      *11. CL WHOLE JOINT COMMISSION RATE               (4)
           03  FILLER      PIC X(8) VALUE "ENNN032 ".
      *12. ACCIDENT & HEALTH                            (5)
           03  FILLER      PIC X(8) VALUE "ENNN032 ".
      *13. PERSONNAL PROPERTY                           (6)
           03  FILLER      PIC X(8) VALUE "ENNN032 ".
      *14. PERSONNAL PROPERTY 2                         (7)
           03  FILLER      PIC X(8) VALUE "ENNN032 ".
      *15. OTHER 1                                      (8)
           03  FILLER      PIC X(8) VALUE "ENNN032 ".
      *16. OTHER 2                                      (9)
           03  FILLER      PIC X(8) VALUE "ENNN032 ".
      *17. OTHER 3                                      (10)
           03  FILLER      PIC X(8) VALUE "ENNN032 ".
      *18. OTHER 4                                      (11)
           03  FILLER      PIC X(8) VALUE "ENNN032 ".
      *19. OTHER 5                                      (12)
           03  FILLER      PIC X(8) VALUE "ENNN032 ".
       01  SPEC-TABLE2    REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 19 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBWI/ICSCANW.CPY".
       01  ICMAIN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/ICMAIN_DEF.CPY".
       COPY "LIBMN/ICMAIN_WKS.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/ICMAIN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ********************************************
       MAIN-PROGRAM SECTION.
      ********************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.               
           MOVE "ICMAIN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS ICMAIN-WINDOW-HANDLE.

           PERFORM OPEN-BR-FILE.
           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO BR-REC
              MOVE EXT-CONAME-USER-LOC TO BR-NO.

           IF BR-AUTO-INS-EARN NOT = "Y"
              MOVE "N" TO CHFG(8)  CHFG(9)  CHFG(10) CHFG(11)
                          CHFG(12) CHFG(13) CHFG(14) CHFG(15)
                          CHFG(16) CHFG(17) CHFG(18) CHFG(19)
           ELSE
              IF BR-OTH-INS-LEVELS NOT = "Y"
                 MOVE "N" TO CHFG(15) CHFG(16) CHFG(17)
                             CHFG(18) CHFG(19).

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-IC-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.


      ************************************
       REC-IC-MODULE SECTION.
      ************************************
       REC-IC-MODULE-NO.
           MOVE 0 TO IC-CODE.
           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

           MOVE "ENTER INSURANCE COMPANY, F6-SCAN" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "EN N020INSCO." TO VDU.
           PERFORM SCREENIO.

           IF ( F5-KEY ) PERFORM ENTER-SEARCHKEY.

           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-IC-MODULE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-IC-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-IC-MODULE-NO.
           MOVE VDUNUM0 TO IC-CODE HOLD-IC-CODE.
           IF IC-CODE = 0
              GO TO REC-IC-MODULE-NO.
       READ-ICFILE.
           PERFORM OPEN-IC1-FILE.
           PERFORM READ-IC1-FILE.
           PERFORM CLOSE-IC1-FILE.
           IF IO-FG = 0
              MOVE IC-REC TO ORIGINAL-IC-REC
              PERFORM DISPLAY-FIELDS
            ELSE
              PERFORM NEW-RECORD-SETUP.
       END-REC-IC-MODULE.
           EXIT.

      ***********************************************
       ENTER-SCANKEY SECTION.
      ***********************************************
           MOVE "F1-RESET, F6-SCAN, <ENTER> TO ACCEPT" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-IC1-FILE.
           MOVE IC-PATH-OWNBR    TO IC-BRNO.
           MOVE 0                TO IC-CODE.


           PERFORM START-IC1-FILE.
       ENTER-KEY.
           MOVE "EN N020SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-ICFILE-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUNUM0 = 0
              IF FIRST-SCAN NOT = 0
                 MOVE IC1-KEY TO HOLD-IC1-KEY
                 MOVE IC-REC TO ORIGINAL-IC-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUNUM0 TO IC-CODE.


           PERFORM START-IC1-FILE.
       READ-ICFILE-NEXT.
           PERFORM READ-IC1-FILE-NEXT.
           IF ( IO-BAD ) OR ( IC-BRNO NOT = IC-PATH-OWNBR )
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
             
      *    UNLOCK IC-FILE.

           MOVE "SN N020INSCO." TO VDU.
           MOVE IC-CODE TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.

           GO TO ENTER-KEY.
       CLOSE-SCANKEY.
           PERFORM CLOSE-IC1-FILE.
           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
       END-SCANKEY.
           EXIT.

      ******************************************************************
       ENTER-SEARCHKEY SECTION.
      ******************************************************************

           MOVE 0     TO ICSCAN-KEY.
           MOVE "WI/ICSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ICSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF (ICSCAN-STATUS NOT = "00") OR (ICSCAN-KEY = ZEROES)
              MOVE "XX" TO VDUSTATUS
              GO TO ENTER-SEARCHKEY-EXIT.

           MOVE "SN N020INSCO." TO VDU.
           MOVE ICSCAN-CODE     TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE ICSCAN-CODE TO VDUNUM0.
           MOVE "00"        TO VDUSTATUS.

       ENTER-SEARCHKEY-EXIT.
           EXIT.

      ******************************************************************
      *       ENTRY-MODULE
      *       NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                  1 - INITIAL NEW RECORD ENTRY
      *                  2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
             MOVE "DEL" TO IO-TYPE
             GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHFG(ELE) = "N"
              GO TO CREATE-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07
                 DEPENDING ON ELE.
           IF ELE > 7
              IF ELE < 20
                 GO TO F-COMM.
           GO TO END-ENTRY-MODULE.
      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUBUF TO IC-NAME.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO IC-ADDR1.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO IC-ADDR2.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO IC-CITY.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO ST1-KEY.
           PERFORM OPEN-ST1-FILE.
           PERFORM READ-ST1-FILE.
           PERFORM CLOSE-ST1-FILE.
           IF IO-FG = 9
             PERFORM INVALID-STATE
             GO TO ENTER-ELE.
           MOVE ST1-KEY TO IC-STATE.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUBUF TO IC-ZIP.
           GO TO CREATE-ROUTINE.
       F-07.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
              MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
              GO TO ENTER-ELE.
           MOVE TEL-BUF TO IC-PHONE.
           PERFORM SEND-PHONE.
           MOVE "S  A120070." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
      *
       F-COMM.
           SUBTRACT 7 FROM ELE GIVING SUB.
           MOVE VDUNUM2 TO IC-COMMRATE(SUB).
       F-COMM1.
           MOVE "E  A010001." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO F-COMM1.
           MOVE VDUBUF TO COMM-TYPE.
           IF NOT (COMM-TYPE = "P" OR "%" OR "F" OR "$")
              MOVE "INVALID COMMISSION TYPE" TO MESS
              PERFORM SEND-MESS
              GO TO F-COMM1.
           IF VDUBUF = "P" OR "%"
              IF IC-COMMRATE(SUB) < 0
                 MULTIPLY -1 BY IC-COMMRATE(SUB).
           IF VDUBUF = "F" OR "$"
              IF IC-COMMRATE(SUB) > 0
                 MULTIPLY -1 BY IC-COMMRATE(SUB).

           MOVE "S  A010001." TO VDU.
           MOVE ELE TO VDUROW.
           IF COMM-TYPE = "P" OR "%"
              IF IC-COMMRATE(SUB) > 100
                 MOVE "PERCENTAGE RATE CANNOT EXCEED 100%" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              END-IF
              MOVE "%" TO VDUBUF
           ELSE
              MOVE "$" TO VDUBUF.
           PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ***************************************
       WRITE-MODULE SECTION.
      ***************************************
           PERFORM OPEN-IC1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.
           MOVE IC-REC TO CHANGED-IC-REC.

           IF ORIGINAL-IC-REC = CHANGED-IC-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-IC1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF IC-REC NOT = ORIGINAL-IC-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-IC-REC TO IC-REC.
           PERFORM REWRITE-IC1-FILE.

           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-IC1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-IC1-FILE.

      ***************************************
       DELETE-MODULE SECTION.
      ***************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-IC1-FILE.
           PERFORM READ-IC1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF IC-REC NOT = ORIGINAL-IC-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           PERFORM DELETE-IC1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-IC1-FILE.

      ***************************************
       NEW-RECORD-SETUP SECTION.
      ***************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE SPACES TO IC-REC.

           MOVE HOLD-IC1-KEY TO IC1-KEY.
           MOVE BR-NO        TO IC-BRNO.
           MOVE 0 TO IC-PHONE.
           MOVE 0 TO IC-COMMRATE(1)  IC-COMMRATE(2)  IC-COMMRATE(3)
                     IC-COMMRATE(4)  IC-COMMRATE(5)  IC-COMMRATE(6)
                     IC-COMMRATE(7)  IC-COMMRATE(8)  IC-COMMRATE(9)
                     IC-COMMRATE(10) IC-COMMRATE(11) IC-COMMRATE(12)
                     IC-COMMRATE(13).
           MOVE 0 TO ELE.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE IC-NAME TO F01.
           MOVE IC-ADDR1 TO F02.
           MOVE IC-ADDR2 TO F03.
           MOVE IC-CITY TO F04.
           MOVE IC-STATE TO F05.
           MOVE IC-ZIP TO F06.
           MOVE IC-PHONE TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT TO F07.
           MOVE IC-COMMRATE(1)              TO F08.
           IF IC-COMMRATE(1) >= 0  MOVE "%" TO F081
           ELSE                    MOVE "$" TO F081.
           MOVE IC-COMMRATE(2)              TO F09.
           IF IC-COMMRATE(2) >= 0  MOVE "%" TO F091
           ELSE                    MOVE "$" TO F091.
           MOVE IC-COMMRATE(3)              TO F10.
           IF IC-COMMRATE(3) >= 0  MOVE "%" TO F101
           ELSE                    MOVE "$" TO F101.
           MOVE IC-COMMRATE(4)              TO F11.
           IF IC-COMMRATE(4) >= 0  MOVE "%" TO F111
           ELSE                    MOVE "$" TO F111.
           MOVE IC-COMMRATE(5)              TO F12.
           IF IC-COMMRATE(5) >= 0  MOVE "%" TO F121
           ELSE                    MOVE "$" TO F121.
           MOVE IC-COMMRATE(6)              TO F13.
           IF IC-COMMRATE(6) >= 0  MOVE "%" TO F131
           ELSE                    MOVE "$" TO F131.
           MOVE IC-COMMRATE(7)              TO F14.
           IF IC-COMMRATE(7) >= 0  MOVE "%" TO F141
           ELSE                    MOVE "$" TO F141.
           MOVE IC-COMMRATE(8)              TO F15.
           IF IC-COMMRATE(8) >= 0  MOVE "%" TO F151
           ELSE                    MOVE "$" TO F151.
           MOVE IC-COMMRATE(9)              TO F16.
           IF IC-COMMRATE(9) >= 0  MOVE "%" TO F161
           ELSE                    MOVE "$" TO F161.
           MOVE IC-COMMRATE(10)             TO F17.
           IF IC-COMMRATE(10) >= 0 MOVE "%" TO F171
           ELSE                    MOVE "$" TO F171.
           MOVE IC-COMMRATE(11)             TO F18.
           IF IC-COMMRATE(11) >= 0 MOVE "%" TO F181
           ELSE                    MOVE "$" TO F181.
           MOVE IC-COMMRATE(12)             TO F19.
           IF IC-COMMRATE(12) >= 0 MOVE "%" TO F191
           ELSE                    MOVE "$" TO F191.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************

           MOVE EXT-CONAME-CONAME   TO ICMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE  TO ICMAIN-SYSDATE.
           DISPLAY SPACES UPON ICMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY ICMAIN-SCREEN UPON ICMAIN-WINDOW-HANDLE.
           MOVE ICMAIN-FORM-FIELDS TO WR-BUF.
           MOVE ICMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE ICMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE ICMAIN-HELP--DIR TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************

           MOVE "00"                 TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.

           EVALUATE TRUE
                    COPY "LIBMN/ICMAIN_EVA.CPY".
                    WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/ACCESS.CPY".
       INVALID-STATE.
           MOVE " INVALID STATE CODE - PLEASE RE-ENTER!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
                                                          TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           PERFORM CALL-TMMSG.

      *********************************************
       IO-ROUTINE SECTION.
      *********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-IC1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPICGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPIC1IN.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *********************************************
       END-ROUTINE SECTION.
      *********************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/LPMNMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ICMAIN-SCREEN.
           DESTROY ICMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
