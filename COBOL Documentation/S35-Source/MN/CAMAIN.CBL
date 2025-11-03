       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CAMAIN.
       DATE-WRITTEN. 10-20-95.
      ******************************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   CASH DRAWER FILE MAINTENANCE
      *                    $$$ = DRAWER PASSWORD OVERRIDE
      *
      * REV:
      *      112497 ADDED LOGIN ID FOR OPENING OF CASH DRAWER
      *  MJD 112497 EXPANDED CA-REC TO 108 BYTES
      *  BAH 062498 CHANGED CA-TILLNO TO ALPHANUMERIC, AND MADE FIRST
      *             CHARACTER CA-TILLNO-ZERO, SO THE OLD TILLNO'S WOULD BE IN
      *             THE CORRECT POSITION, THE OLD TILLNO WAS 99, MUST ALWAYS
      *             MOVE 0 TO THIS NEW FIELD, SO THE OLD KEYS WILL STILL
      *             WORK !!
      *
      *  GLB 990720 ADD CASH DRAWER PASSWORD OPTION TO ENTER DRAWER NO.
      *             BR-PASSWORD-DRAWER(Y/N) & CA-PASSWORD (4 CHAR PASSWORD)
      *             REGMGM PR# 901
      *  GLB 990811 FIX BUG; DONT RE-READ CA-FILE IF NEW-FLAG = 2
      *  090199 GLF 1: CHANGED CASCANW - MOVED WINDOW DOWN 3 LINES
      *             2: CHANGED CASCAN.CMD - MADE SEPARATE LEGENDS FOR EACH
      *                SCAN TYPE
      *             3: CHANGED CASCAN - MADE SURE PROPER LEGEND DISPLAYS FOR
      *                EACH ENTRY FIELD ACCORDING TO SCAN TYPE
      *             PER JTG
      *  020319 MG  CHANGED TO PUT SYSDATE FOR TRANS DATE IF THEY HIT
      *             ENTER-KEY, PER JOHN.
      *  BAH 030127 ADDED CHANGES LOG FOR ANY CHANGES...
      *  AMM 070330 CREATED A LOG FIELD CHANGE ROUTINE
      *  MJD 131126 A30 CONVERSION COMPLETED.
      *  BLM 160920 ADDED CHECK DEPOSIT FIELD ABOVE CASH DEPOSIT, BUMPED
      *             DOWN CASH DEPOSIT & OPENED BY, WORLD PR# 1188
      *   CS 170414 ACTIVATE 8 DIGIT DATES
      *  BLM 211130 FIXED USERID DISPLAYING IN DEPOSIT FIELD ON NEW-
      *             RECORD, REMOVED 2 INCORRECT PERIODS IN DISPLAY-FIELDS.
      *  BAH 2023.1006 CHANGED SCREEN FIELD USERID TO MUST Y TO ALLOW
      *                LOWERCASE
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

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)MN/CAMAIN S35 06/09/23-14:34:43 >".

      ******************************************************************
      *
      *     P R O G R A M - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01DT.CPY".
       COPY "LIBLP/LP01DT_SQL.CPY".
       COPY "LIBLP/LPWSDT.CPY".
       COPY "LIBLP/LP01CA.CPY".
       COPY "LIBLP/LP01CA_SQL.CPY".
       COPY "LIBLP/LPWSCA.CPY".
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".

      *=================================================================
      *   CHCA-REC    CHANGED-CA-REC
      *   ORCA-REC    ORIGINAL-CA-REC
      *=================================================================
       COPY "LIBLP/LP01CA.CPY" REPLACING LEADING "CA" BY "CACH".
       COPY "LIBLP/LP01CA.CPY" REPLACING LEADING "CA" BY "CAOR".

       01  HOLD-CA-REC                 PIC X(CA-SIZE).

      * HOLD-CA1-KEY - CANT USE THIS HERE BECAUSE IT 
      * IS BURIED IN LIBGB/CAPASSWDW.
      * SAVE-CA1-KEY USED INSTEAD OF HOLD-CA1-KEY.
      
       01  SAVE-CA1-KEY.
           03  SAVE-CA-BRNO            PIC 9(04).
           03  SAVE-CA-DATE            PIC 9(08).
           03  SAVE-CA-TILLNO          PIC 9(02).

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
           03  LMC-DRAWER              PIC 99.
           03  FILLER                  PIC X(01).
           03  LMC-DATE                PIC 9(6).
           03  FILLER                  PIC X(01).
           03  LMC-FIELD               PIC X(03).
           03  FILLER                  PIC X(01).
           03  LMC-DESC                PIC X(10).
           03  FILLER                  PIC X(01).

           03  LMC-ORG-TEXT            PIC X(11).
           03  LMC-ORG-AMT             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ,ZZ9.99-.
           03  LMC-ORG-DATE            REDEFINES LMC-ORG-TEXT
                                       PIC 99/99/99.
           03  LMC-ORG-NUM             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZZZZZZ9-.

           03  LMC-TO-X                PIC X(04).

           03  LMC-CHG-TEXT            PIC X(11).
           03  LMC-CHG-AMT             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ,ZZ9.99-.
           03  LMC-CHG-DATE            REDEFINES LMC-CHG-TEXT
                                       PIC 99/99/99.
           03  LMC-CHG-NUM             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZZZZZZ9-.

      ******************************************************************
       01  ERRCD           PIC X      VALUE SPACES.

       01  SUB             PIC 999     VALUE 0.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.

      ******************************************************************
      *
      *    U S E R - I N F O - L I S T
      *
      ******************************************************************

       01  CAPASSWD-MLIST      PIC X(4) VALUE "NEW.".
       01  CAPASSWD-MBUF       PIC X(24).

      ******************************************************************
      *
      *    M I S C - W O R K E R S
      *
      ******************************************************************
       01  EXP-ENDBAL      PIC S9(7)V99   VALUE 0.
       01  CASH-WK         PIC S9(7)V99   VALUE +0.
       01  ENTER-BALBY     PIC X          VALUE "N".
       01  ENTER-DEPOSIT   PIC X          VALUE "N".

       01  OPEN-MSG.
           03  FILLER      PIC X(22) VALUE "DRAWER WILL OPEN WITH ".
           03  OPEN-BAL    PIC $(7).99.

      ******************************************************************
      *
      *    S C R E E N - D I S P L A Y - B U F F E R
      *
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC X(30).
           03  F02         PIC Z,ZZZ,ZZ9.99-.
           03  F021        PIC X(8).
           03  F03         PIC Z,ZZZ,ZZ9.99-.
           03  F04         PIC Z,ZZZ,ZZ9.99-.
           03  F041        PIC Z,ZZZ,ZZ9.99-.
           03  F05         PIC Z,ZZZ,ZZ9.99-.
           03  F051        PIC X(8).
           03  F052        PIC Z,ZZZ,ZZ9.99-.
           03  F06         PIC X(10).
           03  F07         PIC Z,ZZZ,ZZ9.99-.
           03  F08         PIC Z,ZZZ,ZZ9.99-.
           03  F09         PIC X(8).

       01  DISPLAY-LIST.
           03  FILLER      PIC X(52) VALUE
               "010 020 021 030 040 041 050 051 052 060 070 080 090.".

      ******************************************************************
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
      ******************************************************************
       01  MAX             PIC 9   VALUE 9.
       01  SPEC-TABLE.
      * 1. CA-DESC
           03  FILLER      PIC X(8) VALUE "E  A300 ".
      * 2. CA-BEGBAL
           03  FILLER      PIC X(8) VALUE "E  S072 ".
      * 3. CA-RUNNING
           03  FILLER      PIC X(8) VALUE "E  S072 ".
      * 4. CA-NET-CASH-NOTES
           03 FILLER       PIC X(8) VALUE "E  S072 ".
      * 5. CA-ENDBAL
           03  FILLER      PIC X(8) VALUE "E  S072 ".
      * 6. CA-BALBY
           03  FILLER      PIC X(8) VALUE "E  A100 ".
      * 7. CA-CHECK-DEPOSIT
           03  FILLER      PIC X(8) VALUE "E  S072N".
      * 8. CA-DEPOSIT
           03  FILLER      PIC X(8) VALUE "E  S072 ".
      * 9. CA-OPEN-USERID
           03 FILLER       PIC X(8) VALUE "E  A080 ".

       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 9 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CAPASSWDW.CPY".
       COPY "LIBWI/CASCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       01 CAMAIN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/CAMAIN_DEF.CPY".
       COPY "LIBMN/CAMAIN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/CAMAIN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N - P R O G R A M - S E C T I O N
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "CAMAIN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CAMAIN-WINDOW-HANDLE.

       MAIN-MODULE.
      D    STOP MESS.

           PERFORM FORM-RESET.

      * GETTING USERID BRANCH # TO FORCE ONLY FILE CHANGE FOR USER-BR
           IF ( EXT-CONAME-USER-LOC NOT = 0 )
              MOVE "SNNN040BRNO." TO VDU
              MOVE EXT-CONAME-USER-LOC TO VDUNUM0 BR-NO SAVE-CA-BRNO
              PERFORM SCREENIO
              GO TO CHECK-BRANCH.

       ENTER-BRANCH.
           MOVE "ENNN040BRNO." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1>" )
              GO TO END-ROUTINE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-BRANCH.

           MOVE VDUNUM0 TO BR-NO SAVE-CA-BRNO.

      * CHECKING IF VALID BRANCH AND MOVE PATHNAME INFO. FOR CA-FILE
       CHECK-BRANCH.

           PERFORM RECEIVE-BRANCH-MODULE.

           IF ( BR-PATH = SPACES )
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.

           PERFORM OPEN-CA1-FILE.

           MOVE "S  A400BRNAME." TO VDU.
           MOVE BR-NAME        TO VDUBUF.
           PERFORM SCREENIO.

       ENTER-DATE.
           MOVE "ENTER TRANSACTION DATE, F6 - SCAN DRAWER FILE" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060MMDDYY." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1<" )
              PERFORM LOAD-CA1-FILE
              MOVE CA-ALL-PATH      TO CASCAN-CA-PATH
              MOVE SAVE-CA-BRNO     TO CASCANW-BRNO
              MOVE SAVE-CA-DATE     TO CASCAN-DATE
              MOVE SAVE-CA-TILLNO   TO CASCAN-TILLNO
              MOVE "M"              TO CASCAN-TYPE
              MOVE "WI/CASCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH CASCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF ( CASCAN-STATUS NOT = "00" )
                 GO TO ENTER-DATE
              ELSE
                 MOVE CASCAN-CA-KEY  TO SAVE-CA1-KEY
                 MOVE CASCAN-CA-PATH TO CA-ALL-PATH
                 PERFORM DISPLAY-KEY
                 GO TO MAINTENANCE-CA-FILE.

           IF ( VDUSTATUS = "1>" )
              GO TO END-ROUTINE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-DATE.

           MOVE VDU-DATE-YYYYMMDD TO CA-DATE SAVE-CA-DATE.
           IF ( VDU-DATE-YYYYMMDD = 0 )
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8080MMDDYY." TO VDU
              MOVE DATE-YYYYMMDD    TO VDU-DATE-YYYYMMDD
                                       CA-DATE SAVE-CA-DATE
              PERFORM SCREENIO.


       ENTER-TILLNO.

           MOVE "ENTER DRAWER #, F6 - SCAN DRAWER FILE" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "EN N020TILLNO." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1<" )
              MOVE CA-ALL-PATH      TO CASCAN-CA-PATH
              MOVE SAVE-CA-BRNO     TO CASCANW-BRNO
              MOVE SAVE-CA-DATE     TO CASCAN-DATE
              MOVE SAVE-CA-TILLNO   TO CASCAN-TILLNO
              MOVE "M"              TO CASCAN-TYPE
              MOVE "WI/CASCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH CASCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF ( CASCAN-STATUS NOT = "00" )
                 GO TO ENTER-DATE
              ELSE
                 MOVE CASCAN-CA-KEY  TO SAVE-CA1-KEY
                 MOVE CASCAN-CA-PATH TO CA-ALL-PATH
                 PERFORM DISPLAY-KEY
                 GO TO MAINTENANCE-CA-FILE.

           IF ( VDUSTATUS = "1>" )
              GO TO END-ROUTINE.

           IF ( VDUSTATUS = "1U" )
              GO TO ENTER-DATE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-TILLNO.

           MOVE VDUNUM0 TO SAVE-CA-TILLNO CA-TILLNO.
           IF CA-TILLNO = 0
              GO TO ENTER-TILLNO.

      * ONLY ALLOW UP TO 35 FOR NOW BECAUSE IN TRANS POSTING, UNTIL WE HAVE
      * SPACE, THEY ONLY HAVE 1 CHARACTER FOR A DRAWER AND 32 WILL BE "W"
      * 33 WILL BE "X", ETC.. ONLY CAN DO UP TO 35 FOR "Z"
           IF CA-TILLNO > 35
              MOVE "CAN ONLY USE UP TO 35" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TILLNO.

      *----------------------------------------------------------------*
      *    MAIN FUNCTION DRIVING ENTRY,WRITE,REWRITE,DELETE,AND CLEAR
      *----------------------------------------------------------------*
       MAINTENANCE-CA-FILE.

      * VERIFY IF SAVE-CA-BRNO WAS CHANGED FROM WI/CASCAN. 
      *   IF SO, THEN DO NOT ALLOW FURTHER PROCESSING.
      
           IF ( SAVE-CA-BRNO NOT = EXT-CONAME-USER-LOC )
              MOVE "ERROR: BRANCH NUMBER WAS CHANGED" TO MESS
              PERFORM SEND-MESS
              GO TO MAIN-MODULE.
     
           PERFORM READ-CASH-DRAWER-FILE.

           IF BR-PASSWORD-DRAWER NOT = "Y"
              GO TO MAIN-CONTINUE.
           PERFORM ENTER-CASH-DRAWER-PASSWORD.
      * NEED TO RESET SCREEN, GO TO MAIN-MODULE.. IF YOU ENTER AN EXISTING
      * DRAWER BUT DONT KNOW THE PASSWORD, THEIR INFO STAYS ON THE SCREEN
      * WHEN YOU ENTER A NEW DRAWER #. NEEDS TO CLEAR!
           IF CAPASSWD-OKAY = "N"
              GO TO MAIN-MODULE.
           IF NEW-FLAG = 1
              MOVE "S  A000NEW." TO VDU
              MOVE "*** NEW ***" TO VDUBUF
              PERFORM SCREENIO.

       MAIN-CONTINUE.

           PERFORM ENTRY-MODULE.

           IF ( IO-TYPE = "END" )
              GO TO END-ROUTINE.

           IF ( IO-TYPE = "CLR" )
             GO TO MAIN-MODULE.

           IF ( IO-TYPE = "RWR" )
              PERFORM WRITE-MODULE.

           IF ( IO-TYPE = "DEL" )
              PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      ******************************************************************
       READ-CASH-DRAWER-FILE SECTION.
      ******************************************************************

           MOVE SPACES TO CAOR-REC.

           MOVE SAVE-CA1-KEY TO CA1-KEY.
           PERFORM READ-CA1-FILE.
           IF ( IO-FG = 0 )
              MOVE 0      TO NEW-FLAG
              MOVE CA-REC TO CAOR-REC
              PERFORM DISPLAY-FIELDS
              MOVE "O" TO CAPASSWD-STATUS
           ELSE
              MOVE "N" TO CAPASSWD-STATUS
              PERFORM NEW-RECORD-SETUP.

       EXIT-READ-CASH-DRAWER.
           EXIT.

      ******************************************************
       ENTER-CASH-DRAWER-PASSWORD SECTION.
      ******************************************************

       SETUP-CASH-DRAWER-MSG.

      * PASSWORD/TEST THE CASH DRAWER KEY BRNO,DATE,TILL
      *  "N"  =  NEW CASH DRAWER
      *  "O"  =  OLD CASH DRAWER
      *  "M"  =  MODIFY PASSWORD TO OLD DRAWER

           MOVE SPACES TO CAPASSWD-BUF CAPASSWD-OKAY.
           IF CAPASSWD-STATUS = "N"
              MOVE "ENTER DRAWER PASSWORD:" TO CAPASSWD-MBUF
           ELSE
              MOVE "ENTER OLD PASSWORD:" TO CAPASSWD-MBUF.
           MOVE CAPASSWD-MBUF TO WR-BUF.
           MOVE CAPASSWD-MLIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       ENTER-CASH-DRAWER-PASS1.
           MOVE "E  A040CAPASSWD." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUBUF TO CAPASSWD-BUF.

           IF CAPASSWD-STATUS = "N"
              GO TO VERIFY-UNIQUE-PASSWORD
           ELSE
              GO TO TEST-CASH-DRAWER-PASS.

       VERIFY-UNIQUE-PASSWORD.
           MOVE CA-REC  TO HOLD-CA-REC.
           MOVE CA1-KEY TO SAVE-CA1-KEY.

           PERFORM FIND-CASH-DRAWER.
           MOVE SAVE-CA1-KEY TO CA1-KEY.
           IF NEW-FLAG  = 0
              PERFORM OPEN-CA1-FILE
              PERFORM READ-CA1-FILE
              IF IO-FG = 9
                 GO TO IO-ERROR
              END-IF
              PERFORM CLOSE-CA1-FILE.
           MOVE HOLD-CA-REC TO CA-REC.
           IF CAPASSWD-OKAY = "Y"
              GO TO ENTER-CASH-DRAWER-PASS2.
           MOVE "PASSWORD IS NOT UNIQUE, TRY AGAIN!" TO MESS.
           MOVE "N" TO CAPASSWD-OKAY.
           GO TO EXIT-CASH-DRAWER-PASSWORD.
       ENTER-CASH-DRAWER-PASS2.
           MOVE CAPASSWD-BUF TO CA-PASSWORD.
           MOVE "RE-ENTER NEW PASSWORD:" TO CAPASSWD-MBUF.
           IF CAPASSWD-STATUS = "M"
              MOVE "RE-ENTER OLD PASSWORD:" TO CAPASSWD-MBUF.
           MOVE CAPASSWD-MBUF TO WR-BUF.
           MOVE CAPASSWD-MLIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *****CALL RRFORM USING STAT CAPASSWD-LIST CAPASSWD-BUF.
           MOVE "E  A040CAPASSWD." TO VDU.
           PERFORM SCREENIO.
       TEST-CASH-DRAWER-PASS.
           IF CAPASSWD-UNIVERSAL NOT = SPACES
              IF CAPASSWD-BUF = CAPASSWD-UNIVERSAL
                 MOVE CA-PASSWORD TO CAPASSWD-BUF.
           IF CAPASSWD-BUF = CA-PASSWORD
              IF CAPASSWD-STATUS = "M"
                 MOVE "N" TO CAPASSWD-STATUS
                 GO TO SETUP-CASH-DRAWER-MSG
              END-IF
              MOVE "Y" TO CAPASSWD-OKAY
              GO TO EXIT-CASH-DRAWER-PASSWORD.
           MOVE "PASSWORD MISMATCH, TRY AGAIN !!"
              TO MESS PERFORM SEND-MESS.
           MOVE "N" TO CAPASSWD-OKAY.
       EXIT-CASH-DRAWER-PASSWORD.
           MOVE SPACES TO CAPASSWD-BUF CAPASSWD-MBUF WR-BUF.
           MOVE CAPASSWD-ELIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE SPACES TO WR-BUF.
           MOVE CAPASSWD-MLIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************
       FIND-CASH-DRAWER SECTION.
      *****************************************

           PERFORM OPEN-CA1-FILE.

           MOVE SAVE-CA1-KEY  TO CA1-KEY.

           MOVE CA-PATH-OWNBR TO CA-BRNO.
           MOVE 0             TO CA-TILLNO.

           PERFORM START-CA1-FILE.
       NEXT-CA-PASSWORD.
           PERFORM READ-CA1-FILE-NEXT.
           IF IO-FG = 9
              GO TO NO-MATCH-PASSWORD.

           IF CA-BRNO NOT = SAVE-CA-BRNO
              GO TO NO-MATCH-PASSWORD.

           IF CA-DATE NOT = SAVE-CA-DATE
              GO TO NO-MATCH-PASSWORD.

           IF CA-PASSWORD NOT = SPACES
              IF CA-PASSWORD = CAPASSWD-BUF
                 GO TO MATCH-PASSWORD.

           GO TO NEXT-CA-PASSWORD.

       MATCH-PASSWORD.
           IF CA-TILLNO NOT = SAVE-CA-TILLNO
              MOVE "PASSWORD ALREADY RESERVED !" TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO CAPASSWD-OKAY
              GO TO EXIT-FIND-CASH-DRAWER.

           MOVE "Y" TO CAPASSWD-OKAY.
           GO TO EXIT-FIND-CASH-DRAWER.
       NO-MATCH-PASSWORD.
           MOVE "Y" TO CAPASSWD-OKAY.

       EXIT-FIND-CASH-DRAWER.
           PERFORM CLOSE-CA1-FILE.

      ******************************************************************
      *
      *    E N T R Y    M O D U L E
      *
      *        NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                   1 - INITIAL NEW RECORD ENTRY
      *                   2 - MODIFY NEW RECORD
      *
      ******************************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           IF BR-ALLOW-SCANNED-CKS = "Y"
              MOVE " " TO CHFG(7).

       CREATE-ROUTINE.
           IF ( NEW-FLAG NOT = 1 )
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ( ELE < 1 )
              MOVE 1 TO ELE ONE.

           IF ( ELE NOT > MAX )
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

       MODIFY-ROUTINE.
           IF ( ENTER-BALBY = "Y" )
              MOVE "N" TO ENTER-BALBY
              MOVE 6   TO ELE
              GO TO ENTER-ELE.

           IF ( ENTER-DEPOSIT = "Y" )
              MOVE "N" TO ENTER-DEPOSIT
              MOVE 7   TO ELE
              GO TO ENTER-ELE.

           MOVE "<ENTER> = ACCEPT, FIELD #" TO MESS.
           IF BR-PASSWORD-DRAWER = "Y"
              MOVE "<ENTER> = ACCEPT, F5 = PASSWORD, FIELD #" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF  ( VDUSTATUS = "1:" )
              IF BR-PASSWORD-DRAWER = "Y"
                 GO TO MODIFY-DRAWER-PASSWORD.

           IF ( VDUSTATUS NOT = "00" )
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ( ACTION = SPACES )
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( ACTION = "DELETE" )
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.

           IF ( CHNO NOT NUMERIC )
              GO TO MODIFY-ROUTINE.

           IF ( CHNO = 0 )
              GO TO MODIFY-ROUTINE.

           IF ( CHNO > MAX )
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

       ENTER-ELE.

      * CHECK TO SEE IF OPENING A DRAWER, IF SO DON'T ACCESS 2 THRU 7
           IF ( NEW-FLAG NOT = 0 )
              IF ( ELE = 2 OR 3 OR 4 OR 5 OR 6 OR 7 OR 8 OR 9)
                 GO TO CREATE-ROUTINE.

           IF ( CHKEY NOT = ZEROS )
              IF ( CHFG(ELE) = " " )
                 IF ( NEW-FLAG NOT = 1 )
                    GO TO MODIFY-ROUTINE.

           IF ( NEW-FLAG = 0 )
              IF ( CHFG(ELE) = "N" )
                 IF ( CHKEY NOT = EXT-CONAME-DAILY-PASSWORD )
                    GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE)  TO VDU.
           MOVE ELE        TO VDUROW.
           MOVE 0          TO VDUCOL.
           MOVE "."        TO VDUDECIMAL.
           PERFORM SCREENIO.

       TEST-F4.
           IF ( VDUSTATUS = "1U" )
              IF ( NEW-FLAG = 1 )
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.

       TEST-1D.
           IF ( VDUSTATUS = "1D" )
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

       TEST-F1.
           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

       F-01.
           MOVE VDUBUF TO CA-DESC.

           IF ( VDUBUF = " " )
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       
       F-02.
           MOVE VDUNUM2       TO CA-BEGBAL.
           PERFORM TIME-CALL.
           MOVE H-AND-M       TO CA-OPENED-HHMM.
           PERFORM CONVERT-TIME.
           MOVE "S  A000021." TO VDU.
           MOVE TIME-EDIT     TO VDUBUF.
           PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.
       
       F-03.
           MOVE VDUNUM2 TO CA-RUNNING.
           GO TO CREATE-ROUTINE.

       F-04.
           MOVE VDUNUM2 TO CA-NET-CASH-NOTES.
       F-041.
           COMPUTE EXP-ENDBAL =  CA-BEGBAL
                               + CA-RUNNING
                               + CA-NET-CASH-NOTES.
           MOVE "S  S072041." TO VDU.
           MOVE EXP-ENDBAL    TO VDUNUM2.
           PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.

       F-05.
           MOVE VDUNUM2       TO CA-ENDBAL.
           PERFORM TIME-CALL.
           MOVE H-AND-M       TO CA-CLOSED-HHMM.
           PERFORM CONVERT-TIME.
           MOVE "S  A000051." TO VDU.
           MOVE TIME-EDIT     TO VDUBUF.
           PERFORM SCREENIO.

       F-052.
           COMPUTE CASH-WK =     CA-ENDBAL - CA-BEGBAL
                                           - CA-RUNNING
                                           - CA-NET-CASH-NOTES.
           MOVE "S  S072052." TO VDU.
           MOVE CASH-WK       TO VDUNUM2.
           PERFORM SCREENIO.

           MOVE "Y" TO ENTER-BALBY.
           GO TO CREATE-ROUTINE.

       
       F-06.
           MOVE VDUBUF TO CA-BALBY.

           IF ( VDUBUF = " " )
              GO TO ENTER-ELE.

           MOVE "Y" TO ENTER-DEPOSIT.
           GO TO CREATE-ROUTINE.

       
       F-07.
           MOVE VDUNUM2 TO CA-CHECK-DEPOSIT.

           IF ( CA-CHECK-DEPOSIT > CA-ENDBAL )
              MOVE "CAN NOT BE MORE THAN ENDING BALANCE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           COMPUTE OPEN-BAL = CA-ENDBAL
                            - CA-DEPOSIT
                            - CA-CHECK-DEPOSIT.
           MOVE OPEN-MSG TO MESS.
           PERFORM SEND-MESS.
          
           GO TO CREATE-ROUTINE.
       
       F-08.
           MOVE VDUNUM2 TO CA-DEPOSIT.

           IF ( CA-DEPOSIT > CA-ENDBAL )
              MOVE "CAN NOT BE MORE THAN ENDING BALANCE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF BR-ALLOW-SCANNED-CKS = "Y"
              COMPUTE OPEN-BAL = CA-ENDBAL
                               - CA-DEPOSIT
                               - CA-CHECK-DEPOSIT
           ELSE
              COMPUTE OPEN-BAL = CA-ENDBAL - CA-DEPOSIT.

           MOVE OPEN-MSG TO MESS.
           PERFORM SEND-MESS.

           GO TO CREATE-ROUTINE.

       F-09.
           MOVE VDUBUF TO CA-OPEN-USERID.
           GO TO CREATE-ROUTINE.

       
       MODIFY-DRAWER-PASSWORD.

           MOVE "M" TO CAPASSWD-STATUS.
           PERFORM ENTER-CASH-DRAWER-PASSWORD.
           IF CAPASSWD-OKAY = "N"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE
           ELSE
              GO TO MODIFY-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-CA1-FILE.

           IF ( NEW-FLAG NOT = 0 )
              GO TO WRITE-RECORD.

           MOVE CA-REC TO CACH-REC.

           IF ( CAOR-REC = CACH-REC )
              GO TO END-WRITE-MODULE.

           PERFORM READ-CA1-FILE.

           IF ( IO-FG NOT = 0 )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF ( CA-REC NOT = CAOR-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE CACH-REC TO CA-REC.
           PERFORM REWRITE-CA1-FILE.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-CA1-FILE.
           IF ( IO-FG NOT = 0 )
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-CA1-FILE.

      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE BR-NO                      TO LMC-BRNO.
           MOVE CA-TILLNO                  TO LMC-DRAWER.
           MOVE CA-DATE                    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO LMC-DATE.
           MOVE " TO "                     TO LMC-TO-X.
       
           IF CAOR-DESC NOT = CACH-DESC
             MOVE "01 "                   TO LMC-FIELD
             MOVE "DESC"                  TO LMC-DESC
             MOVE CAOR-DESC               TO LMC-ORG-TEXT
             MOVE CACH-DESC               TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF CAOR-BEGBAL NOT = CACH-BEGBAL
             MOVE "02 "                   TO LMC-FIELD
             MOVE "BEGBAL  "              TO LMC-DESC
             MOVE CAOR-BEGBAL             TO LMC-ORG-AMT
             MOVE CACH-BEGBAL             TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF CAOR-OPENED-HHMM NOT = CACH-OPENED-HHMM
             MOVE "021"                   TO LMC-FIELD
             MOVE "OPENED  "              TO LMC-DESC
             MOVE CAOR-OPENED-HHMM        TO LMC-ORG-NUM
             MOVE CACH-OPENED-HHMM        TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF CAOR-RUNNING NOT = CACH-RUNNING
             MOVE "03 "                   TO LMC-FIELD
             MOVE "RUNNING "              TO LMC-DESC
             MOVE CAOR-RUNNING            TO LMC-ORG-AMT
             MOVE CACH-RUNNING            TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF CAOR-NET-CASH-NOTES NOT = CACH-NET-CASH-NOTES
             MOVE "04 "                   TO LMC-FIELD
             MOVE "NETCASH "              TO LMC-DESC
             MOVE CAOR-NET-CASH-NOTES     TO LMC-ORG-AMT
             MOVE CACH-NET-CASH-NOTES     TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF CAOR-ENDBAL NOT = CACH-ENDBAL
             MOVE "05 "                   TO LMC-FIELD
             MOVE "ENDBAL  "              TO LMC-DESC
             MOVE CAOR-ENDBAL             TO LMC-ORG-AMT
             MOVE CACH-ENDBAL             TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF CAOR-CLOSED-HHMM NOT = CACH-CLOSED-HHMM
             MOVE "051"                   TO LMC-FIELD
             MOVE "CLOSED  "              TO LMC-DESC
             MOVE CAOR-CLOSED-HHMM        TO LMC-ORG-NUM
             MOVE CACH-CLOSED-HHMM        TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF CAOR-BALBY NOT = CACH-BALBY
             MOVE "06 "                   TO LMC-FIELD
             MOVE "BALBY   "              TO LMC-DESC
             MOVE CAOR-BALBY              TO LMC-ORG-TEXT
             MOVE CACH-BALBY              TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF CAOR-CHECK-DEPOSIT NOT = CACH-CHECK-DEPOSIT 
             MOVE "07 "                   TO LMC-FIELD
             MOVE "CHK DEP "              TO LMC-DESC
             MOVE CAOR-CHECK-DEPOSIT      TO LMC-ORG-AMT
             MOVE CACH-CHECK-DEPOSIT      TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF CAOR-DEPOSIT NOT = CACH-DEPOSIT
             MOVE "08 "                   TO LMC-FIELD
             MOVE "DEPOSIT "              TO LMC-DESC
             MOVE CAOR-DEPOSIT            TO LMC-ORG-AMT
             MOVE CACH-DEPOSIT            TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF CAOR-OPEN-USERID NOT = CACH-OPEN-USERID
             MOVE "09 "                   TO LMC-FIELD
             MOVE "OPEN USER"             TO LMC-DESC
             MOVE CAOR-OPEN-USERID        TO LMC-ORG-TEXT
             MOVE CACH-OPEN-USERID        TO LMC-CHG-TEXT
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
       DELETE-MODULE SECTION.
      ******************************************************************
           IF ( NEW-FLAG NOT = 0 )
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-CA1-FILE.
           PERFORM READ-CA1-FILE.

           IF ( IO-FG NOT = 0 )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF ( CA-REC NOT = CAOR-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-CA1-FILE.

      *WRITE CHANGES FILE
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY
           STRING CA1-KEY, " ", "DELETED" DELIMITED BY SIZE
                INTO ERRLOGW-KEY.

           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       END-DELETE-MODULE.
           PERFORM CLOSE-CA1-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1                    TO NEW-FLAG.
           MOVE "S  A000NEW."        TO VDU.
           MOVE "*** NEW DRAWER ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-CA-FILE.
           MOVE SAVE-CA1-KEY TO CA1-KEY.
           MOVE 0 TO ELE.

           IF ( BR-AUTO-OPEN-BAL = "Y" ) AND CA-TILLNO NUMERIC
       
              MOVE "SN S072020." TO VDU
              MOVE CA-TILLNO TO SUB
              MOVE BR-OPEN-BAL(SUB) TO CA-BEGBAL VDUNUM2
              PERFORM SCREENIO
              PERFORM TIME-CALL
              MOVE H-AND-M       TO CA-OPENED-HHMM
              PERFORM CONVERT-TIME
              MOVE "S  A000021." TO VDU
              MOVE TIME-EDIT     TO VDUBUF
              PERFORM SCREENIO.

           MOVE "S  A080090." TO VDU.
           MOVE EXT-CONAME-USER-ID TO VDUBUF  CA-OPEN-USERID.
           PERFORM SCREENIO.


      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES     TO DISPLAY-BUF.

           MOVE CA-DESC    TO F01.
           MOVE CA-BEGBAL  TO F02.
           MOVE CA-OPENED-HHMM  TO H-AND-M.
           PERFORM CONVERT-TIME.
           MOVE TIME-EDIT  TO F021.
           MOVE CA-RUNNING TO F03.
           MOVE CA-NET-CASH-NOTES TO F04.
           ADD CA-BEGBAL CA-RUNNING CA-NET-CASH-NOTES GIVING F041.
           MOVE CA-OPEN-USERID TO F09.

           IF ( CA-CLOSED-HHMM NOT = 0 )
              MOVE CA-CLOSED-HHMM TO H-AND-M
              PERFORM CONVERT-TIME
              MOVE TIME-EDIT TO F051.

           IF ( CA-BALBY NOT = " " )
              MOVE CA-ENDBAL  TO F05
              COMPUTE CASH-WK = CA-ENDBAL - CA-BEGBAL
                                          - CA-RUNNING
                                          - CA-NET-CASH-NOTES
              MOVE CASH-WK    TO F052
              MOVE CA-BALBY   TO F06
              MOVE CA-CHECK-DEPOSIT TO F07
              MOVE CA-DEPOSIT TO F08.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-KEY SECTION.
      ******************************************************************
           MOVE "SNNN040BRNO."   TO VDU.
           MOVE SAVE-CA-BRNO     TO VDUNUM0.
           PERFORM SCREENIO.
       
           MOVE "S  A080MMDDYY." TO VDU.
           MOVE SAVE-CA-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.
       
           MOVE "SNNN020TILLNO." TO VDU.
           MOVE SAVE-CA-TILLNO   TO VDUNUM0.
           PERFORM SCREENIO.

      ***************************************************************
       FORM-RESET SECTION.
      ***************************************************************
           MOVE EXT-CONAME-CONAME TO CAMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CAMAIN-SYSDATE.
           DISPLAY SPACES UPON CAMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CAMAIN-SCREEN UPON CAMAIN-WINDOW-HANDLE.
           MOVE CAMAIN-FORM-FIELDS TO WR-BUF.
           MOVE CAMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ***************************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************************
           MOVE CAMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CAMAIN-HELP--DIR TO HELP-LINK-DIR.

      ***************************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/CAMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       RECEIVE-BRANCH-MODULE SECTION.
      ******************************************************************
     
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-BAD )
              MOVE SPACES                  TO BR-PATH
              GO TO RECEIVE-BRANCH-MODULE-EXIT.
     
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.
     
           MOVE "B"                        TO CA-PATH-OVERRIDE.
     
       RECEIVE-BRANCH-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPCACL.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
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
       TIME-CALL.
           ACCEPT TIME-NOW FROM TIME.
       CONVERT-TIME.
           IF ( H-AND-M NOT GREATER THAN 1159 )
              MOVE "AM" TO T-AMPM
           ELSE
              MOVE "PM" TO T-AMPM.

           IF ( H-AND-M IS NOT LESS THAN 1300 )
              SUBTRACT 1200 FROM H-AND-M.

           IF ( CUR-HH = 0 )
              MOVE 12 TO CUR-HH.

           MOVE CUR-HH TO T-HH.
           MOVE CUR-MM TO T-MM.
           MOVE ":"    TO T-COL.
           MOVE SPACE  TO T-SPACE.
       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS           TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPDTGS_SQL.CPY".
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPCA1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DESTROY CAMAIN-WINDOW-HANDLE
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
