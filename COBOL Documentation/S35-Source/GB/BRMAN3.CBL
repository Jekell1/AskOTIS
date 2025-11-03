      * A30 CONVERSION COMPLETE
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BRMAN3.
      **************************************************************************
      *
      *  DIR :   GB
      *  DESC:   BRANCH FILE MAINTENANCE PAGE 3
      *
      * REV:
      *  JTG 051997 REMOVED REFERENCE TO O4-O7 INSURANCE
      *  MJD 112197 EXPANDED BR-REC TO 850 BYTES
      *  KEC 121797 ADDED PAGE 7.
      *  MJD 031398 ADDED 78 LEVEL: BR-SIZE
      *  BAH 990413 ADDED CALL TO CREATE CHANGES FILE AFTER REWRITE
      *  JTG 990618 ADDED GP-I-AM-REGMGM, REGIONAL MANAGEMENT, SC
      *             WHICH WILL ALLOW SELECTING BR-INS-ADDON 'Y' OR 'O'
      *             WHEN BR-REB-METHOD-.() = 4 OR 5
      *  MG  991005 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BAH 991025 PARAM-ENTRY-2 SECTION DIDNT ALLOW FORMULA > 6, JUDY, TFC
      *  BAH 010621 CHANGED BR-REB-METHOD-2 SUBSCRIPT 4 FROM O6 TO CL LEVEL,
      *             LOAN REC ONLY HAS O1 - O5, REGWAN WANTS DIFFERENT METHODS
      *             FOR DECREASING -VS- LEVEL, PR# 1500
      *  AMM 070307 CREATED A LOG FIELD CHANGE ROUTINE
      *  BAH 120514 REMOVED SPECIAL-FG
      *   CS 160121 REMOVED BR-REB-METHOD-2, CHANGED CL(LEV) TO BR-REB-METHOD(7)      *             O3 TO BR-REB-METHOD(6)
      *             
      *             WHEN NEW INSURANCE ADDED MAP SUB TO  ELE
      *   IN A30 
      *    CL(DEC)  -  1      O2       - 5     
      *    AH       -  2      O3       - 6     O5     -  9 
      *    PP       -  3      CL(LEV)  - 7     O6     - 10      
      *    O1       -  4      O4       - 8     O7     - 11
      *   CS 20160121 REMOVED LTFILE, USED LTI, ADDED PRU_LNK TO LINKAGE
      *  BAH 20211116 LIMIT ACCESS BY PASSWORD TO ALL FIELDS, #1524
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

      ******************************************************************
      *
      *      W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GB/BRMAN3.CBL S35 09/20/22-12:47:31 >".
      ******************************************************************
      *
      *      P R O G R A M     W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ORIGINAL LOAN RECORD
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/GB01BR.CPY" REPLACING
            LEADING "BR" BY "BROR".
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CHANGED LOAN RECORD
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/GB01BR.CPY" REPLACING
            LEADING "BR" BY "BRCH".

      ******************************************************************
      *
      *    L M C - W O R K E R S
      *
      *=================================================================
      *    DESC: LOG-MAINTENANCE-CHGS WORKERS
      ******************************************************************
       01  LMC-DETAIL                  PIC X(55)      VALUE SPACES.
       01  FILLER                      REDEFINES LMC-DETAIL.
           03  LMC-BRNO                PIC 9(04).
           03  FILLER                  PIC X(01).
           03  LMC-FIELD               PIC X(03).
           03  FILLER                  PIC X(01).
           03  LMC-DESC                PIC X(09).
           03  FILLER                  PIC X(01).

           03  LMC-ORG-TEXT            PIC X(16).
           03  LMC-ORG-AMT             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-ORG-DATE            REDEFINES LMC-ORG-TEXT
                                       PIC 99/99/99.
           03  LMC-ORG-DLNO            REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ9.99.
           03  LMC-ORG-GLNO            REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZ/ZZZZZZ9.
           03  LMC-ORG-NUM             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-ORG-PHONE           REDEFINES LMC-ORG-TEXT
                                       PIC 999/999/9999.
           03  LMC-ORG-SSNO            REDEFINES LMC-ORG-TEXT
                                       PIC 999/99/9999.

           03  LMC-TO-X                PIC X(04).

           03  LMC-CHG-TEXT            PIC X(16).
           03  LMC-CHG-AMT             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-CHG-DATE            REDEFINES LMC-CHG-TEXT
                                       PIC 99/99/99.
           03  LMC-CHG-DLNO            REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ9.99.
           03  LMC-CHG-GLNO            REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZ/ZZZZZZ9.
           03  LMC-CHG-NUM             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-CHG-PHONE           REDEFINES LMC-CHG-TEXT
                                       PIC 999/999/9999.
           03  LMC-CHG-SSNO            REDEFINES LMC-CHG-TEXT
                                       PIC 999/99/9999.

      ******************************************************************

       01  SUB             PIC 9(2)    VALUE 0.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).

       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).

       01  FORM-NUM REDEFINES ACTION.
           03  FORM-KEY    PIC X(4).
           03  FORM-P      PIC X.
           03  FORM-SEL    PIC 9.

       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.

       01  ORIGINAL-BR-REC    PIC X(BR-SIZE).
       01  CHANGED-BR-REC     PIC X(BR-SIZE).

       01  BRMAIN-LINK       VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(4).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMNUM       PIC 9.
           03  BRMAIN-WINDOW-HANDLE PIC X(10).

      ******************************************************************
      *    S C R E E N    D I S P L A Y   B U F F E R
      ******************************************************************
       01  DISPLAY-BUF          VALUE SPACES.
           03  DISPLAY-TABLE    OCCURS 7 TIMES.
               05  F01          PIC 9.
               05  F02          PIC Z9.
               05  F03          PIC ZZ9.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(36) VALUE
           "010 011 012 020 021 022 030 031 032 ".
           03  FILLER      PIC X(36) VALUE
           "040 041 042 050 051 052 060 061 062 ".
           03  FILLER      PIC X(12) VALUE
           "070 071 072.".

      ******************************************************************
      *       ELEMENT SPECIFICATIONS #1
      ******************************************************************
       01  MAX            PIC 99   VALUE 7.
       01  SPEC-TABLE.
      * 1. CL (DECR)
           03  FILLER     PIC X(8) VALUE "ENNN010 ".
      * 2. CL (LEVL)
           03  FILLER     PIC X(8) VALUE "ENNN010 ".
      * 3. A & H
           03  FILLER     PIC X(8) VALUE "ENNN010 ".
      * 4. PP
           03  FILLER     PIC X(8) VALUE "ENNN010 ".
      * 5. O1
           03  FILLER     PIC X(8) VALUE "ENNN010 ".
      * 6. O2
           03  FILLER     PIC X(8) VALUE "ENNN010 ".
      * 7. O3
           03  FILLER     PIC X(8) VALUE "ENNN010 ".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 7 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ******************************************************************
      *
      *    C O P Y   L I B R A R Y   W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/PASSWDW.CPY".
       78  FIELDS-SEQ                VALUE 0.
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/BRMAN3_DEF.CPY".
       COPY "LIBGB/BRMAN3_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).
       SCREEN SECTION.
       COPY "LIBGB/BRMAN3_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BRMAN3" TO VDU-FORM-NAME.

           MOVE FORM-LINK-AREA TO BRMAIN-LINK.

       MAIN-MODULE.
           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

           PERFORM FORM-RESET.

           PERFORM REC-BR-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
      D    STOP MESS.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P
              MOVE 4 TO FORM-SEL.

           IF FORM-SEL NOT = 0
              IF FORM-P = "P"
                 MOVE BR-NO TO SAVEKEY
                 MOVE FORM-SEL TO FORMNUM
                 GO TO END-ROUTINE.

           MOVE "N" TO NEW-REC.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-BR-MODULE SECTION.
      ******************************************************************
       REC-BR-MODULE-NO.
           MOVE SPACES TO IO-TYPE.

           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO BR-NO
              MOVE " " TO SAVEKEY
              MOVE "SN N040BRANCH." TO VDU
              MOVE BR-NO TO VDUNUM0
              PERFORM SCREENIO
              GO TO READ-BRANCH.

           MOVE 0 TO NEW-FLAG.
           MOVE "EN N040BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BR-MODULE.

           IF NOT ENTER-KEY
              GO TO REC-BR-MODULE-NO.

           IF VDUNUM0 = 0
              GO TO REC-BR-MODULE-NO.

           MOVE VDUNUM0 TO BR-NO.

      *----------------------------------------------------------------
       READ-BRANCH.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              GO TO REC-BR-MODULE-NO.

           MOVE BR-REC TO ORIGINAL-BR-REC.

           PERFORM DISPLAY-FIELDS.

           MOVE 0 TO NEW-FLAG.
           IF NEW-REC = "Y"
              MOVE 1 TO NEW-FLAG.

      *----------------------------------------------------------------
       END-REC-BR-MODULE.
           EXIT.

      ******************************************************************
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      *
      ******************************************************************
       ENTRY-MODULE SECTION.
      ******************************************************************
       BEGIN-ENTRY.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
      *----------------------------------------------------------------
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ELE < 1
              MOVE 1 TO ELE ONE.

           IF ELE NOT > MAX
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

      *----------------------------------------------------------------
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF FORM-KEY = " " AND FORM-P = "P" AND FORM-SEL NUMERIC
              IF NOT (FORM-SEL = 1 OR 2 OR 4 OR 5 OR 6 OR 7)
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "RWR" TO IO-TYPE
                 GO TO EXIT-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.

           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.

           IF CHNO = 0
              GO TO MODIFY-ROUTINE.

           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      * LIMIT ACCESS TO ALL FIELDS 
           IF NEW-FLAG = 0
              MOVE FIELDS-SEQ                    TO PASSWDW-SEQ
              MOVE "TO CHANGE THIS FIELD......." TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE.

      *----------------------------------------------------------------
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       TEST-F4.
           IF UP-KEY
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       TEST-F5.
           IF DOWN-KEY
              MOVE 1 TO ONE
              GO TO GOTO-FIELD1.

      *----------------------------------------------------------------
       TEST-F1.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *----------------------------------------------------------------
       GOTO-FIELD1.
           GO TO F-01 F-01 F-01 F-01 F-01 F-01 F-01
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *****************************************************************
      *    S E T   R E C O R D   E L E M E N T S
      *****************************************************************
       F-01.
           IF ( ELE = 1 OR 3 OR 4 OR 5 OR 6)
              PERFORM PARAM-ENTRY 
           ELSE
           IF (ELE = 2 OR 7)
              PERFORM PARAM-ENTRY-2.

           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF IO-BAD
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL.

      *----------------------------------------------------------------
       EXIT-ENTRY-MODULE.
           EXIT.

      ******************************************************************
      *
      *    PARAM-ENTRY SECTION.     ELE = 1 CL(DECR)
      *                                   3 A&H
      *                                   4 PP
      *                                   5 O1
      *                                   6 O2
      *
      * CL(DECR), A&H, PP, O1, O2
      *    1       2   3   4   5   (SUB) FOR BR-REB-METHOD
      *--------------------------------------------------------- 
      *   IN A30, WHEN 04 - 07 INSURANCE ADDED, REBATE-METHOD WILL
      *           MATCH ELE.              
      *    CL(DEC)  -  1      O2       - 5     
      *    AH       -  2      O3       - 6     O5     -  9 
      *    PP       -  3      CL(LEV)  - 7     O6     - 10      
      *    O1       -  4      O4       - 8     O7     - 11
      *   CS 160121 REMOVED LTFILE, USED LTI, ADDED PRU_LNK TO LINKAGE

      ******************************************************************
       PARAM-ENTRY SECTION.
      ******************************************************************
           MOVE 0 TO IO-FG.

      * THE REB-METHOD TABLE DOES NOT MATCH 'ELE' ANYMORE (6/21/01)
      * ADDED CL(LEVL) IN THE MIDDLE OF IT !! REGWAN PR# 1500
      * 1/21/16
      * WHEN NEW INSURANCES ARE ADDED (OT4 +, ELE WILL MATCH REB-METHOD)

           MOVE 1 TO SUB.
           IF ELE > 1 
              COMPUTE SUB = ELE - 1.

           IF ( VDUNUM0 = 4 OR 5 )
             IF BR-INS-ADDON = "Y"
               MOVE "OPTIONS 4&5 ARE NOT AVAILABLE WITH ADDONS" TO MESS
               PERFORM SEND-MESS
               MOVE 9 TO IO-FG
               GO TO EXIT-PARAM-ENTRY.

           IF ( VDUNUM0 = 0 OR 3 OR 4 OR 5 )
              MOVE VDUNUM0 TO BR-REB-METHOD(SUB)
              MOVE "SNNN020  1." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO BR-REBDAYS-MONTH(SUB) VDUNUM0
              PERFORM SCREENIO
              MOVE "S  N030  2." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO BR-ACCRUAL-MONTHS(SUB) VDUNUM0
              PERFORM SCREENIO
              MOVE SPACE TO MESS
              PERFORM SEND-LEGEND
              GO TO EXIT-PARAM-ENTRY.

           IF VDUNUM0 > 9
              MOVE "MUST BE 1 - 9" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
              GO TO EXIT-PARAM-ENTRY.

           IF VDUNUM0 = 8
              MOVE "ONLY AVAILABLE IN REL A50" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
              GO TO EXIT-PARAM-ENTRY.

           MOVE VDUNUM0 TO BR-REB-METHOD(SUB).

      *----------------------------------------------------------------
       ENTER-REBDAYS-MONTH.
           MOVE "ENTER 0 FOR NO SWING, OR 1 TO 31 FOR SWING" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.

           IF F1-KEY
              MOVE SPACE TO MESS
              PERFORM SEND-LEGEND
              MOVE 8 TO IO-FG
              GO TO EXIT-PARAM-ENTRY.

           IF UP-KEY
              MOVE SPACE TO MESS
              PERFORM SEND-LEGEND
              MOVE 9 TO IO-FG
              GO TO EXIT-PARAM-ENTRY.

           IF NOT ENTER-KEY
              GO TO ENTER-REBDAYS-MONTH.

           IF VDUNUM0 < 0 OR VDUNUM0 > 31
              MOVE "MUST BE BETWEEN 0 AND 31 DAYS" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-REBDAYS-MONTH.

           MOVE VDUNUM0 TO BR-REBDAYS-MONTH(SUB).

      *----------------------------------------------------------------
       ENTER-ACCRUAL-MONTHS.
           MOVE "ENTER NUMBER OF MONTHS TO ACCRUE, 0 FOR TERM" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.

           IF F1-KEY
              MOVE SPACE TO MESS
              PERFORM SEND-LEGEND
              MOVE 8 TO IO-FG
              GO TO EXIT-PARAM-ENTRY.

           IF UP-KEY
              MOVE SPACE TO MESS
              PERFORM SEND-LEGEND
              GO TO ENTER-REBDAYS-MONTH.

           IF NOT ENTER-KEY
              MOVE SPACE TO MESS
              PERFORM SEND-LEGEND
              GO TO ENTER-ACCRUAL-MONTHS.

           MOVE VDUNUM0 TO BR-ACCRUAL-MONTHS(SUB).

      *----------------------------------------------------------------
       EXIT-PARAM-ENTRY.
           MOVE SPACE TO MESS.
           PERFORM SEND-LEGEND.

      ******************************************************************
      *
      *    PARAM-ENTRY-2 SECTION.   ELE = 2 CL(LEVL)
      *                                   7 O3
      *
      *    O3,  CL (LEVL)
      *    1       2               (SUB) FOR O3 BR-REB-METHOD = 6
      *                            (SUB) FOR CL(LEVL)BR-REB-METHOD = 7
      ******************************************************************
       PARAM-ENTRY-2 SECTION.
      ******************************************************************
           MOVE 0 TO IO-FG.

           IF ELE = 2
             MOVE 7 TO SUB
           ELSE
             MOVE 6 TO SUB.


           IF ( VDUNUM0 = 4 OR 5 )
             IF BR-INS-ADDON = "Y"
               MOVE "OPTIONS 4&5 ARE NOT AVAILABLE WITH ADDONS" TO MESS
               PERFORM SEND-MESS
               MOVE 9 TO IO-FG
               GO TO EXIT-PARAM-ENTRY-2.

           IF ( VDUNUM0 = 0 OR 3 OR 4 OR 5 )
              MOVE VDUNUM0 TO BR-REB-METHOD(SUB)
              MOVE "SNNN020  1." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO BR-REBDAYS-MONTH(SUB) VDUNUM0
              PERFORM SCREENIO
              MOVE "S  N030  2." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO BR-ACCRUAL-MONTHS(SUB) VDUNUM0
              PERFORM SCREENIO
              MOVE SPACE TO MESS
              PERFORM SEND-LEGEND
              GO TO EXIT-PARAM-ENTRY-2.

           IF VDUNUM0 > 9
              MOVE "MUST BE 1 - 9" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
              GO TO EXIT-PARAM-ENTRY-2.

           IF VDUNUM0 = 8
              MOVE "ONLY AVAILABLE IN REL A50" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
              GO TO EXIT-PARAM-ENTRY-2.
 
           MOVE VDUNUM0 TO BR-REB-METHOD(SUB).

      *---------------------------------------------------------------
       ENTER-REBDAYS-MONTH-2.
           MOVE "ENTER 0 FOR NO SWING, OR 1 TO 31 FOR SWING" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.

           IF F1-KEY
              MOVE SPACE TO MESS
              PERFORM SEND-LEGEND
              MOVE 8 TO IO-FG
              GO TO EXIT-PARAM-ENTRY-2.

           IF UP-KEY
              MOVE SPACE TO MESS
              PERFORM SEND-LEGEND
              MOVE 9 TO IO-FG
              GO TO EXIT-PARAM-ENTRY-2.

           IF NOT ENTER-KEY
              GO TO ENTER-REBDAYS-MONTH-2.

           IF VDUNUM0 < 0 OR VDUNUM0 > 31
              MOVE "MUST BE BETWEEN 0 AND 31 DAYS" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-REBDAYS-MONTH-2.

           MOVE VDUNUM0 TO BR-REBDAYS-MONTH(SUB).

      *---------------------------------------------------------------
       ENTER-ACCRUAL-MONTHS-2.
           MOVE "ENTER NUMBER OF MONTHS TO ACCRUE, 0 FOR TERM" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.

           IF F1-KEY
              MOVE SPACE TO MESS
              PERFORM SEND-LEGEND
              MOVE 8 TO IO-FG
              GO TO EXIT-PARAM-ENTRY-2.

           IF UP-KEY
              MOVE SPACE TO MESS
              PERFORM SEND-LEGEND
              GO TO ENTER-REBDAYS-MONTH-2.
 
           IF NOT ENTER-KEY
              MOVE SPACE TO MESS
              PERFORM SEND-LEGEND
              GO TO ENTER-ACCRUAL-MONTHS-2.

           MOVE VDUNUM0 TO BR-ACCRUAL-MONTHS(SUB).

      *---------------------------------------------------------------
       EXIT-PARAM-ENTRY-2.
           MOVE SPACE TO MESS.
           PERFORM SEND-LEGEND.

      ******************************************************************
       VERIFY-PASSWORD SECTION.
      ******************************************************************
           MOVE " "                          TO PASSWDW-STAT.
           MOVE "BRMAN3"                     TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD"                  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       EXIT-VERIFY-PASSWORD.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
       REWRITE-RECORD.
           PERFORM OPEN-BR-FILE.
           MOVE BR-REC TO CHANGED-BR-REC.

           IF ORIGINAL-BR-REC = CHANGED-BR-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-BR-FILE.

           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF BR-REC NOT = ORIGINAL-BR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE CHANGED-BR-REC TO BR-REC.
           PERFORM REWRITE-BR-FILE.

      * CODE TO WRITE CHANGE FILE.
      *    MOVE "C" TO ERRLOGW-ACTION.
      *    MOVE BR-NO TO ERRLOGW-KEY.
      *    MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
      *    MOVE "CL/ERRLOG" TO FORM-NAM.
      *    CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
      *    CANCEL FORM-PROGX.

       END-WRITE-MODULE.
           PERFORM CLOSE-BR-FILE.

      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************

           MOVE ORIGINAL-BR-REC               TO BROR-REC.
           MOVE CHANGED-BR-REC                TO BRCH-REC.

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE BR-NO                      TO LMC-BRNO.
           MOVE " TO "                     TO LMC-TO-X.

      * CL (DECR)- - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF BROR-REB-METHOD(1) NOT = BRCH-REB-METHOD(1)
             MOVE "01 "                   TO LMC-FIELD
             MOVE "DCL METHD"             TO LMC-DESC
             MOVE BROR-REB-METHOD(1)      TO LMC-ORG-NUM
             MOVE BRCH-REB-METHOD(1)      TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-REBDAYS-MONTH(1) NOT = BRCH-REBDAYS-MONTH(1)
             MOVE "02 "                   TO LMC-FIELD
             MOVE "DCL RBDAY"             TO LMC-DESC
             MOVE BROR-REBDAYS-MONTH(1)   TO LMC-ORG-NUM
             MOVE BRCH-REBDAYS-MONTH(1)   TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ACCRUAL-MONTHS(1) NOT = BRCH-ACCRUAL-MONTHS(1)
             MOVE "03 "                   TO LMC-FIELD
             MOVE "DCL ACCRU"             TO LMC-DESC
             MOVE BROR-ACCRUAL-MONTHS(1)  TO LMC-ORG-NUM
             MOVE BRCH-ACCRUAL-MONTHS(1)  TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

      * CL (LEVL)- - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF BROR-REB-METHOD(7) NOT = BRCH-REB-METHOD(7)
             MOVE "01 "                   TO LMC-FIELD
             MOVE "LCL METHD"             TO LMC-DESC
             MOVE BROR-REB-METHOD(7)      TO LMC-ORG-NUM
             MOVE BRCH-REB-METHOD(7)      TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-REBDAYS-MONTH(7) NOT = BRCH-REBDAYS-MONTH(7)
             MOVE "02 "                   TO LMC-FIELD
             MOVE "LCL RBDAY"             TO LMC-DESC
             MOVE BROR-REBDAYS-MONTH(7)   TO LMC-ORG-NUM
             MOVE BRCH-REBDAYS-MONTH(7)   TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ACCRUAL-MONTHS(7) NOT = BRCH-ACCRUAL-MONTHS(7)
             MOVE "03 "                    TO LMC-FIELD
             MOVE "LCL ACCRU"              TO LMC-DESC
             MOVE BROR-ACCRUAL-MONTHS(7)   TO LMC-ORG-NUM
             MOVE BRCH-ACCRUAL-MONTHS(7)   TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

      * A & H- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF BROR-REB-METHOD(2) NOT = BRCH-REB-METHOD(2)
             MOVE "01 "                   TO LMC-FIELD
             MOVE "AH METHOD"             TO LMC-DESC
             MOVE BROR-REB-METHOD(2)      TO LMC-ORG-NUM
             MOVE BRCH-REB-METHOD(2)      TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-REBDAYS-MONTH(2) NOT = BRCH-REBDAYS-MONTH(2)
             MOVE "02 "                   TO LMC-FIELD
             MOVE "AH REBDAY"             TO LMC-DESC
             MOVE BROR-REBDAYS-MONTH(2)   TO LMC-ORG-NUM
             MOVE BRCH-REBDAYS-MONTH(2)   TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ACCRUAL-MONTHS(2) NOT = BRCH-ACCRUAL-MONTHS(2)
             MOVE "03 "                   TO LMC-FIELD
             MOVE "AH ACCRUE"             TO LMC-DESC
             MOVE BROR-ACCRUAL-MONTHS(2)  TO LMC-ORG-NUM
             MOVE BRCH-ACCRUAL-MONTHS(2)  TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

      * PP - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF BROR-REB-METHOD(3) NOT = BRCH-REB-METHOD(3)
             MOVE "01 "                   TO LMC-FIELD
             MOVE "PP METHOD"             TO LMC-DESC
             MOVE BROR-REB-METHOD(3)      TO LMC-ORG-NUM
             MOVE BRCH-REB-METHOD(3)      TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-REBDAYS-MONTH(3) NOT = BRCH-REBDAYS-MONTH(3)
             MOVE "02 "                   TO LMC-FIELD
             MOVE "PP REBDAY"             TO LMC-DESC
             MOVE BROR-REBDAYS-MONTH(3)   TO LMC-ORG-NUM
             MOVE BRCH-REBDAYS-MONTH(3)   TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ACCRUAL-MONTHS(3) NOT = BRCH-ACCRUAL-MONTHS(3)
             MOVE "03 "                   TO LMC-FIELD
             MOVE "PP ACCRUE"             TO LMC-DESC
             MOVE BROR-ACCRUAL-MONTHS(3)  TO LMC-ORG-NUM
             MOVE BRCH-ACCRUAL-MONTHS(3)  TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

      * O1 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF BROR-REB-METHOD(4) NOT = BRCH-REB-METHOD(4)
             MOVE "01 "                   TO LMC-FIELD
             MOVE "O1 METHOD"             TO LMC-DESC
             MOVE BROR-REB-METHOD(4)      TO LMC-ORG-NUM
             MOVE BRCH-REB-METHOD(4)      TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-REBDAYS-MONTH(4) NOT = BRCH-REBDAYS-MONTH(4)
             MOVE "02 "                   TO LMC-FIELD
             MOVE "O1 REBDAY"             TO LMC-DESC
             MOVE BROR-REBDAYS-MONTH(4)   TO LMC-ORG-NUM
             MOVE BRCH-REBDAYS-MONTH(4)   TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ACCRUAL-MONTHS(4) NOT = BRCH-ACCRUAL-MONTHS(4)
             MOVE "03 "                   TO LMC-FIELD
             MOVE "O1 ACCRUE"             TO LMC-DESC
             MOVE BROR-ACCRUAL-MONTHS(4)  TO LMC-ORG-NUM
             MOVE BRCH-ACCRUAL-MONTHS(4)  TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

      * O2 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF BROR-REB-METHOD(5) NOT = BRCH-REB-METHOD(5)
             MOVE "01 "                   TO LMC-FIELD
             MOVE "O2 METHOD"             TO LMC-DESC
             MOVE BROR-REB-METHOD(5)      TO LMC-ORG-NUM
             MOVE BRCH-REB-METHOD(5)      TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-REBDAYS-MONTH(5) NOT = BRCH-REBDAYS-MONTH(5)
             MOVE "02 "                   TO LMC-FIELD
             MOVE "O2 REBDAY"             TO LMC-DESC
             MOVE BROR-REBDAYS-MONTH(5)   TO LMC-ORG-NUM
             MOVE BRCH-REBDAYS-MONTH(5)   TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ACCRUAL-MONTHS(5) NOT = BRCH-ACCRUAL-MONTHS(5)
             MOVE "03 "                   TO LMC-FIELD
             MOVE "O2 ACCRUE"             TO LMC-DESC
             MOVE BROR-ACCRUAL-MONTHS(5)  TO LMC-ORG-NUM
             MOVE BRCH-ACCRUAL-MONTHS(5)  TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

      * O3 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF BROR-REB-METHOD(6) NOT = BRCH-REB-METHOD(6)
             MOVE "01 "                   TO LMC-FIELD
             MOVE "O3 METHOD"             TO LMC-DESC
             MOVE BROR-REB-METHOD(6)      TO LMC-ORG-NUM
             MOVE BRCH-REB-METHOD(6)      TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-REBDAYS-MONTH(6) NOT = BRCH-REBDAYS-MONTH(6)
             MOVE "02 "                   TO LMC-FIELD
             MOVE "O3 REBDAY"             TO LMC-DESC
             MOVE BROR-REBDAYS-MONTH(6)   TO LMC-ORG-NUM
             MOVE BRCH-REBDAYS-MONTH(6)   TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ACCRUAL-MONTHS(6) NOT = BRCH-ACCRUAL-MONTHS(6)
             MOVE "03 "                    TO LMC-FIELD
             MOVE "O3 ACCRUE"              TO LMC-DESC
             MOVE BROR-ACCRUAL-MONTHS(6)   TO LMC-ORG-NUM
             MOVE BRCH-ACCRUAL-MONTHS(6)   TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

       LOG-MAINTENANCE-CHGS-EXIT.
           EXIT.

      ******************************************************************
       LOG-FIELD-CHANGE SECTION.
      ******************************************************************

           MOVE "F"           TO ERRLOGW-ACTION.
           MOVE LMC-DETAIL    TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       LOG-FIELD-CHANGE-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES               TO DISPLAY-BUF.
      * CL (DECR)
           MOVE BR-REB-METHOD(1)      TO F01(1).
           MOVE BR-REBDAYS-MONTH(1)   TO F02(1).
           MOVE BR-ACCRUAL-MONTHS(1)  TO F03(1).

      * CL (LEVL)
           MOVE BR-REB-METHOD(7)     TO F01(2).
           MOVE BR-REBDAYS-MONTH(7)  TO F02(2).
           MOVE BR-ACCRUAL-MONTHS(7) TO F03(2).

      * A&H
           MOVE BR-REB-METHOD(2)      TO F01(3).
           MOVE BR-REBDAYS-MONTH(2)   TO F02(3).
           MOVE BR-ACCRUAL-MONTHS(2)  TO F03(3).

      * PP
           MOVE BR-REB-METHOD(3)      TO F01(4).
           MOVE BR-REBDAYS-MONTH(3)   TO F02(4).
           MOVE BR-ACCRUAL-MONTHS(3)  TO F03(4).

      * O1
           MOVE BR-REB-METHOD(4)      TO F01(5).
           MOVE BR-REBDAYS-MONTH(4)   TO F02(5).
           MOVE BR-ACCRUAL-MONTHS(4)  TO F03(5).

      * O2
           MOVE BR-REB-METHOD(5)      TO F01(6).
           MOVE BR-REBDAYS-MONTH(5)   TO F02(6).
           MOVE BR-ACCRUAL-MONTHS(5)  TO F03(6).

      * O3
           MOVE BR-REB-METHOD(6)      TO F01(7).
           MOVE BR-REBDAYS-MONTH(6)   TO F02(7).
           MOVE BR-ACCRUAL-MONTHS(6)  TO F03(7).

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".


      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO BRMAN3-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BRMAN3-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BRMAN3-SCREEN.
           DISPLAY BRMAN3-SCREEN UPON BRMAIN-WINDOW-HANDLE.
           MOVE BRMAN3-FORM-FIELDS TO WR-BUF.
           MOVE BRMAN3-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE BRMAN3-HELP--FILE TO HELP-LINK-FILE.
           MOVE BRMAN3-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/BRMAN3_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
                                                  TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRIN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BRMAN3-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE BRMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
