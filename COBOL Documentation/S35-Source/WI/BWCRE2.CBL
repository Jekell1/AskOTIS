       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BWCRE2.
       DATE-WRITTEN.    021898.
      **************************************************************************
      *   BORROWER CREATE   F5-CREATE BORROWER FROM LOAN INPUT
      *                          PAGE 2
      *                        (WINDOW)
      * REV:
      *  MJD 000502 ADDED LOGIC FOR CONTACT PHONE NUMBERS IN BM-REC (TFC PR#37)
      *  MJD 000608 ADDED GP-REF-PHONE TO BLOCK ENTRY OF CONTACT PHONE #'S
      *             (PR#37)
      *  BAH 021007 WAS NOT CLEARING OUT THE PHONE EXTENSION WHEN SPACING OUT
      *             THE DESCRIPTION.
      *  BAH 030402 ADDED BX-FILE (BORROWER TRAILER) AND FIELD 17. EMAIL,
      *             LENDMARK PR# 2044
      *  BAH 030626 CREATE A CHANGES REC AND BORROWER MEMO WHEN FIELDS ARE
      *             CHANGED, LENDMARK
      *  BAH 060802 ADDED BW-BAD-MISC-PHONE1-FG AND PHONE2-FG, REGACC PR# 142
      *  BAH 100106 ADDED PASSWORD ROUTINE SEQ 10 FOR WORLD MEXICO PR# 667
      *  BAH 120418 ADDED P17 PROMPT FOR SPANISH VS ENGLISH, WORLD PR# 800
      *  BAH 120608 REMOVED ALL NONDISPLAY FIELDS FROM FORM
      *  BAH 120705 CHANGED PROMPT 5. MISC. PHONE 5 AND 6 TO FIELDS FOR
      *             REGACC TO HAVE DIFFERENT PROMPT, REGACC PR# 315
      *  BAH 130121 A30 CONVERSION
      *  BAH 150928 CHANGED MISC PHONE PROMPTS FOR WORLD MEXICO PR# 1103
      *
      * KEC 2016.0328 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      *
      *  BAH 160822 REMOVED HOLD-CONTACT-DESC, PHONE AND EXT, MOVED FROM
      *             MEMO TO BW-REC
      * BAH 170119 ADDED LEGEND-01-SPANISH FOR WORLD PR#1218
      * BAH 20221024 PASSWORD FIELD 16 CONTACT PHONE AS THEY STORE THE
      *              BUYERS NAME FOR BUYBAC AND METRO2 IN THERE #1574
      * BAH 2023.1006 CHANGED SCREEN FIELD EMAIL TO MUST Y TO ALLOW
      *               LOWERCASE
      * BAH 2024.1204 PASSWORD CHANGING REFERENCES IF CEASE & DESIST #1686
      *                                                           S35Q-145
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      **************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/BWCRE2.CBL S34 05/24/21-15:20:33 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       01  ERRCD               PIC X     VALUE SPACES.

       01  TEST-CEASE-DESIST   PIC X(14).
       01  TEST-PHONE          PIC 9(10).
           88  BAD-PHONE       VALUE 0001111111 1111111111
                                     0002222222 2222222222
                                     0003333333 3333333333
                                     0004444444 4444444444
                                     0005555555 5555555555
                                     0006666666 6666666666
                                     0007777777 7777777777
                                     0008888888 8888888888
                                     0009999999 9999999999
                                     0000000000.

       01  BWCRE2-LINES          PIC 99  VALUE 22.
       01  BWCRE2-COLUMNS        PIC 99  VALUE 78.
       01  BWCRE2-BEGIN-Y        PIC 99  VALUE 2.
       01  BWCRE2-BEGIN-X        PIC 99  VALUE 2.

       01  LEGEND          PIC X(35).
       01  LEGEND-01.
           03  FILLER      PIC X(21)  VALUE
           "F1, F7-EXIT, ENTER #:".
       01  LEGEND-01-SPANISH.
           03  FILLER      PIC X(24)  VALUE
           "F1, F7-SALIR, # DE FILA:".
       01  LEGEND-02.
           03  FILLER      PIC X(8)  VALUE
           " F7-EXIT".

       01  EXT-EOBUF-MESS      PIC X.

       01  BWCRE2-WINDOW-HANDLE        PIC X(10).
       01  CHAN                       PIC X(4).
       01  SCAN-ERR        PIC X      VALUE "Y".
       01  SUB             PIC 999    COMP  VALUE 0.
       01  ONE             PIC S9     COMP.
       01  ELE             PIC 9(2)   COMP.
       01  NEW-FLAG        PIC 9.
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).

       01  IO-TYPE         PIC X(3).

      *************************************
      *   SCREEN DISPLAY BUFFER
      *************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  DISPLAY-BUFX OCCURS 6 TIMES.
               05  D-PHONE         PIC X(12).
               05  D-EXT           PIC Z(4).
               05  D-PHONE-CD      PIC XX.
               05  D-PHONE-DESC    PIC X(20).
           03  DISPLAY-CONTACT OCCURS 10 TIMES.
               05  D-CONTACT-DESC  PIC X(46).
               05  D-CONTACT-PHONE PIC X(12).
               05  D-CONTACT-EXT   PIC Z(4).
           03  F17                 PIC X(40).
           03  D-BAD-PHONE-1       PIC X.
           03  D-BAD-PHONE-2       PIC X.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(32) VALUE
           "010 011 012 013 020 021 022 023 ".
           03  FILLER      PIC X(32) VALUE
           "030 031 032 033 040 041 042 043 ".
           03  FILLER      PIC X(32) VALUE
           "050 051 052 053 060 061 062 063 ".
           03  FILLER      PIC X(36) VALUE
           "070 071 072 080 081 082 090 091 092 ".
           03  FILLER      PIC X(36) VALUE
           "100 101 102 110 111 112 120 121 122 ".
           03  FILLER      PIC X(36) VALUE
           "130 131 132 140 141 142 150 151 152 ".
           03  FILLER      PIC X(24) VALUE
           "160 161 162 170 014 024.".

       01  MAX             PIC 99   VALUE 17.

       01  SPEC-TABLE.
      *01. MISC PHONE NUMBER #1
           03  FILLER      PIC X(8) VALUE "E  A012 ".
      *02. MISC PHONE NUMBER #2
           03  FILLER      PIC X(8) VALUE "E  A012 ".
      *03. MISC PHONE NUMBER #3
           03  FILLER      PIC X(8) VALUE "E  A012 ".
      *04. MISC PHONE NUMBER #4
           03  FILLER      PIC X(8) VALUE "E  A012 ".
      *05. MISC PHONE NUMBER #5
           03  FILLER      PIC X(8) VALUE "E  A012 ".
      *06. MISC PHONE NUMBER #6
           03  FILLER      PIC X(8) VALUE "E  A012 ".
      *07. CONTACT DESC #1
           03  FILLER      PIC X(8) VALUE "E  A460 ".
      *08. CONTACT DESC #2
           03  FILLER      PIC X(8) VALUE "E  A460 ".
      *09. CONTACT DESC #3
           03  FILLER      PIC X(8) VALUE "E  A460 ".
      *10. CONTACT DESC #4
           03  FILLER      PIC X(8) VALUE "E  A460 ".
      *11. CONTACT DESC #5
           03  FILLER      PIC X(8) VALUE "E  A460 ".
      *12. CONTACT DESC #6
           03  FILLER      PIC X(8) VALUE "E  A460 ".
      *13. CONTACT DESC #7
           03  FILLER      PIC X(8) VALUE "E  A460 ".
      *14. CONTACT DESC #8
           03  FILLER      PIC X(8) VALUE "E  A460 ".
      *15. CONTACT DESC #9
           03  FILLER      PIC X(8) VALUE "E  A460 ".
      *16. CONTACT DESC #10
           03  FILLER      PIC X(8) VALUE "E  A460 ".
      *17. EMAIL
           03  FILLER      PIC X(8) VALUE "E  A400 ".
       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 17 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       01  LEGEND-WALK.
           03  FILLER                   PIC X(10)  VALUE "F1, ^-UP, ".
           03  FILLER                   PIC X      VALUE X'76'.
           03  FILLER                   PIC X(24)  VALUE
           "-DOWN, ENTER FIELD INFO:".

       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/DATERW.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/BWCRE2_DEF.CPY".
       COPY "LIBWI/BWCRE2_WKS.CPY".

       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/PASSWDW.CPY".
      * WANT TO BE THE SAME AS PAGE 1 (MODBOR,CHBORR)
       78  PHONE-16               VALUE 16.
       78  CEASE-DESIST-SEQ       VALUE 17.
 
      *=================================================================
      * ORIG-BW-REC FROM 'REPLACING LEADING' ON BW-REC - ORIGINAL-BW-REC
      *=================================================================
       COPY "LIBLP/LP01BW.CPY" REPLACING LEADING "BW-" BY "ORIG-BW-".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LP01BW.CPY".
       01  ALLOW-CHANGES              PIC X.
       SCREEN SECTION.
       COPY "LIBWI/BWCRE2_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME
                  BW-REC
                  ALLOW-CHANGES 
                  EXIT-PATHNAME.                

      **************************************
       MAIN-PROGRAM SECTION.
      **************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "BWCRE2" TO VDU-FORM-NAME.

              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE BWCRE2-BEGIN-Y * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  BWCRE2-BEGIN-X * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    BWCRE2-COLUMNS + VDU-WIN-W-MOD
                      LINES   BWCRE2-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS BWCRE2-WINDOW-HANDLE.


           MOVE " " TO VDU-WIMENU-SELECTION.

      * STORE ORIGINAL BW-REC FROM PAGE 1:
           MOVE BW-REC TO ORIG-BW-REC.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              MOVE " " TO BR-CHANGE-MEMO.

           PERFORM GET-GPENV.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           IF GP-REF-PHONE NOT = "Y"
              PERFORM PROTECT-REF-PHONE.

           PERFORM DISPLAY-FIELDS.
           MOVE 0 TO NEW-FLAG.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

      * RESTORE ORIGINAL BW-REC FROM PAGE 1:
           IF IO-TYPE = "CLR"
              MOVE ORIG-BW-REC TO BW-REC.

           GO TO MAIN-MODULE.


      *************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      *************************************************
       ENTRY-MODULE SECTION.
           MOVE " " TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE " " TO IO-TYPE.
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
      * ALLOW MODIFY OF BRANCH
           IF ALLOW-CHANGES = "Y"
              PERFORM TEST-LEGEND-01
           ELSE
              MOVE LEGEND-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           IF ALLOW-CHANGES = "N"
              PERFORM ALARM
              MOVE "PLEASE NOTE..." TO ERRMSG-HEAD
              MOVE "CHANGES ARE NOT ALLOWED FROM THIS SCREEN!!"
                                            TO ERRMSG-MSG1
              MOVE SPACES TO ERRMSG-MSG2 ERRMSG-MSG3
              PERFORM CALL-ERRMSG
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.
           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.

           IF ELE = 16
              MOVE PHONE-16                 TO PASSWDW-SEQ
              MOVE "TO CHANGE CONTACT 16.." TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE.

      * CEASE & DESIST FIELDS THAT GET SET WITH THE UPDATE
      * CANT CHANGE WITHOUT A PASSWORD
      * REFERENCES

      * IF WRITTEN, YOU SHOULDNT BE ABLE TO CHANGE ANY, THEY WILL ALL
      * BE SPACES AND THE FIRST ONE ONLY WILL SAY "CEASE & DESIST"
           IF NEW-FLAG = 0
              IF ( (ELE = 7 OR 8 OR 9 OR 10 OR 11 OR 12) AND
                    (BW-CEASE-DESIST-TYPE = "W") )
                       MOVE CEASE-DESIST-SEQ         TO PASSWDW-SEQ
                       MOVE "TO CHANGE DUE TO CEASE" TO PASSWDW-PROMPT2
                       PERFORM VERIFY-PASSWORD-MUST-EXIST
                       IF PASSWDW-STAT NOT = "OK"
                          GO TO MODIFY-ROUTINE.

      * IF REFERENCE OR COMBO, YOU SHOULDNT BE ABLE TO CHANGE THE ONES THAT
      * SAY "CEASE & DESIST" AS YOU CAN SELECT WHICH REFERENCE
           IF NEW-FLAG = 0
              IF ( (ELE = 7 OR 8 OR 9 OR 10 OR 11 OR 12) AND
               (BW-CEASE-DESIST-TYPE = "R" OR "C") )
                  COMPUTE SUB = ELE - 6
                  MOVE BW-CONTACT-DESC(SUB) TO TEST-CEASE-DESIST
                  IF TEST-CEASE-DESIST = "CEASE & DESIST"
                       MOVE CEASE-DESIST-SEQ         TO PASSWDW-SEQ
                       MOVE "TO CHANGE DUE TO CEASE" TO PASSWDW-PROMPT2
                       PERFORM VERIFY-PASSWORD-MUST-EXIST
                       IF PASSWDW-STAT NOT = "OK"
                          GO TO MODIFY-ROUTINE.

       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG (ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 IF CHKEY NOT = CHAN
                    GO TO MODIFY-ROUTINE.
           IF NEW-FLAG = 1
              MOVE LEGEND-WALK TO LEGEND
           ELSE
              MOVE "F1-CANCEL, ENTER FIELD INFO:" TO LEGEND.
           PERFORM SEND-LEGEND.
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
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-MISC F-MISC F-MISC F-MISC F-MISC F-MISC
                 F-CONTACT F-CONTACT F-CONTACT F-CONTACT F-CONTACT
                 F-CONTACT F-CONTACT F-CONTACT F-CONTACT F-CONTACT
                 F-17 DEPENDING ON ELE.

           GO TO EXIT-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************

       F-MISC.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
               MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
               GO TO ENTER-ELE.
           MOVE TEL-BUF TO BW-MISC-PHONE(ELE).
           PERFORM SEND-PHONE.
           MOVE "S  A120  0." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
           IF BW-MISC-PHONE(ELE) = 0
              MOVE "SNNN040  1." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO BW-MISC-EXT(ELE) VDUNUM0
              PERFORM SCREENIO
              MOVE "S  A020  2." TO VDU
              MOVE ELE TO VDUROW
              MOVE SPACES TO BW-MISC-PHONE-CD(ELE) VDUBUF
              PERFORM SCREENIO
              MOVE "S  A014  3." TO VDU
              MOVE ELE TO VDUROW
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A010  4." TO VDU
              MOVE ELE TO VDUROW
              IF ELE = 1
                 MOVE SPACES TO BW-BAD-MISC-PHONE1-FG VDUBUF
                 PERFORM SCREENIO
              ELSE
              IF ELE = 2
                 MOVE SPACES TO BW-BAD-MISC-PHONE2-FG VDUBUF
                 PERFORM SCREENIO
              END-IF
              GO TO CREATE-ROUTINE.
           IF GP-USE-BW-BAD-FLAGS = "Y" AND (ELE = 1 OR 2)
              MOVE BW-MISC-PHONE(ELE) TO TEST-PHONE
              IF BAD-PHONE
                 IF ELE = 1
                    MOVE "S  A010014." TO VDU
                    MOVE "Y" TO BW-BAD-MISC-PHONE1-FG VDUBUF
                    PERFORM SCREENIO
                 ELSE
                    MOVE "S  A010024." TO VDU
                    MOVE "Y" TO BW-BAD-MISC-PHONE2-FG VDUBUF
                    PERFORM SCREENIO.
       F-EXT.
           MOVE "ENNN040  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-EXT.
           MOVE VDUNUM0 TO BW-MISC-EXT(ELE).
       F-CODE.
           MOVE "E  A020  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-EXT.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF SCAN-ERR = "Y"
                 GO TO F-CODE
              ELSE
                 MOVE "00" TO VDUSTATUS
                 MOVE CDSCAN-CODE TO VDUBUF.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-CODE.

           MOVE "PH" TO CD-TYPE.
           MOVE VDUBUF TO BW-MISC-PHONE-CD(ELE) CD-CODE.
           PERFORM GET-CD.
           MOVE "S  A014  3." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CD-DESC TO VDUBUF.
           PERFORM SCREENIO.
           IF IO-FG NOT = 0
              GO TO F-CODE.

       F-BAD.
           IF GP-USE-BW-BAD-FLAGS = "Y"
              IF ELE = 1 OR 2
                 MOVE "E  A010  4." TO VDU
                 MOVE ELE TO VDUROW
                 PERFORM SCREENIO
                 IF VDUSTATUS = "1U"
                    GO TO F-CODE
                 ELSE
                    IF VDUBUF NOT = "Y" AND NOT = "N"
                       GO TO F-BAD
                    ELSE
                       IF ELE = 1
                          MOVE VDUBUF TO BW-BAD-MISC-PHONE1-FG
                       ELSE
                          MOVE VDUBUF TO BW-BAD-MISC-PHONE2-FG.

           GO TO CREATE-ROUTINE.

       F-CONTACT.
           COMPUTE SUB = ELE - 6.
           MOVE VDUBUF TO BW-CONTACT-DESC(SUB).
           IF BW-CONTACT-DESC(SUB) = SPACES
              MOVE 0 TO BW-CONTACT-PHONE(SUB)
              MOVE "S  A020  1." TO VDU
              MOVE ELE TO VDUROW
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "SNNN040  2." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO BW-CONTACT-EXT(SUB) VDUNUM0
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
       F-CONTACT-PHONE.
           MOVE "E  A120  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-CONTACT-PHONE.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
               MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
               GO TO F-CONTACT-PHONE.
           MOVE TEL-BUF TO BW-CONTACT-PHONE(SUB).
           PERFORM SEND-PHONE.
           MOVE "S  A120  1." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
       F-CONTACT-EXT.
           MOVE "ENNN040  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-CONTACT-PHONE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-CONTACT-EXT.
           MOVE VDUNUM0 TO BW-CONTACT-EXT(SUB).
           GO TO CREATE-ROUTINE.

       F-17.
           MOVE VDUBUF TO BW-EMAIL.
           GO TO CREATE-ROUTINE.

       EXIT-ENTRY-MODULE.
           EXIT.

      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************
           MOVE SPACES TO DISPLAY-BUF.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF BW-MISC-PHONE(SUB) NOT = 0
                 MOVE BW-MISC-PHONE(SUB) TO TEL-BUF
                 PERFORM SEND-PHONE
                 MOVE TEL-EDIT TO D-PHONE(SUB)
                 MOVE BW-MISC-EXT(SUB) TO D-EXT(SUB)
                 MOVE "PH" TO CD-TYPE
                 MOVE BW-MISC-PHONE-CD(SUB) TO D-PHONE-CD(SUB)
                                               CD-CODE
                 PERFORM GET-CD
                 MOVE CD-DESC TO D-PHONE-DESC(SUB)
              END-IF
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF BW-CONTACT-DESC(SUB) NOT = SPACES
                 IF BW-CONTACT-PHONE(SUB) = 0
                    MOVE SPACES TO D-CONTACT-PHONE(SUB)
                 ELSE
                    MOVE BW-CONTACT-PHONE(SUB) TO TEL-BUF
                    PERFORM SEND-PHONE
                    MOVE TEL-EDIT TO D-CONTACT-PHONE(SUB)
                 END-IF
                 MOVE BW-CONTACT-EXT(SUB) TO D-CONTACT-EXT(SUB)
                 MOVE BW-CONTACT-DESC(SUB) TO D-CONTACT-DESC(SUB)
              END-IF
           END-PERFORM.

           IF BR-STATE = "MX"
              MOVE "S  A070P17." TO VDU
              MOVE "FUENTE:" TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A200MISC1." TO VDU
              MOVE "1. CEL REFERENCIA 1:" TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A200MISC2." TO VDU
              MOVE "2. CEL REFERENCIA 2:" TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A200MISC3." TO VDU
              MOVE "3. CEL REFERENCIA 3:" TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A200MISC4." TO VDU
              MOVE "4. CEL REFERENCIA 4:" TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A200MISC5." TO VDU
              MOVE "5. CEL REFERENCIA 5:" TO VDUBUF
              PERFORM SCREENIO.
           MOVE BW-EMAIL TO F17.
           MOVE BW-BAD-MISC-PHONE1-FG TO D-BAD-PHONE-1.
           MOVE BW-BAD-MISC-PHONE2-FG TO D-BAD-PHONE-2.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "BWCRE2" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      **************************************
       VERIFY-PASSWORD-MUST-EXIST SECTION.
      **************************************
           MOVE "EN"                         TO PASSWDW-STAT.
           MOVE "BWCRE2"                     TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ***************************************
       CODE-SCAN SECTION.
      ***************************************
           MOVE "Y" TO SCAN-ERR.

           MOVE "PH"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
               MOVE "Y" TO SCAN-ERR
               GO TO EXIT-CODE-SCAN.

           MOVE "S  A020  2." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CDSCAN-CODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "PH"        TO CD-TYPE.
           MOVE CDSCAN-CODE TO CD-CODE.
           PERFORM GET-CD.

           MOVE "S  A014  3." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CD-DESC TO  VDUBUF.
           PERFORM SCREENIO.

           MOVE "N" TO SCAN-ERR.

       EXIT-CODE-SCAN.
           EXIT.

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BWCRE2-SCREEN.
           DISPLAY BWCRE2-SCREEN UPON BWCRE2-WINDOW-HANDLE.
           MOVE BWCRE2-FORM-FIELDS TO WR-BUF.
           MOVE BWCRE2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE BWCRE2-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWCRE2-HELP--DIR TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/BWCRE2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************
       MISC-PARAGRAPHS SECTION.
      *****************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETDAT.CPY".
       TEST-LEGEND-01.
           IF BR-STATE = "MX"
              MOVE LEGEND-01-SPANISH TO LEGEND
           ELSE
              MOVE LEGEND-01 TO LEGEND.
 
       PROTECT-REF-PHONE.
           MOVE "N" TO CHFG(07) CHFG(08) CHFG(09) CHFG(10) CHFG(11)
                       CHFG(12) CHFG(13) CHFG(14) CHFG(15) CHFG(16).
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID CODE" TO CD-DESC.
       SEND-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE LEGEND TO WR-BUF.
           PERFORM CALL-WRFORM.

      ******************************
       IO-ROUTINE SECTION.
      ******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".

      ******************************
       END-ROUTINE SECTION.
      ******************************
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BWCRE2-SCREEN.
           DESTROY BWCRE2-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
