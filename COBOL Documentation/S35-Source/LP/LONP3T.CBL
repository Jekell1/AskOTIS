       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONP3T.
       DATE-WRITTEN.    112697.
      ******************************************************************
      *
      *  DESCRIPTION: CHECK PRINT
      *
      *  CUSTOMERS  : WORLD ACCEPTANCE
      *
      *  NOTES      : THE GLNO'S AND DESCRIPTIONS WILL PRINT ON THE VOUCHER.
      *                 DOT MATRIX - CHECK-PRINT SECTION.
      *                              WRITE-GL-INFO SECTION.
      *                              ALIGNMENT-PRINT SECTION.
      *
      *                 LASER PRTR - PCL-CHECK-PRINT SECTION.
      *                              PCL-WRITE-GL-INFO SECTION.
      *                              PCL-ALIGNMENT-PRINT SECTION.
      *
      *
      * REV :
      *  052295 BLL MATCH DT-SEQ TO CX-DT-SEQ FOR MULTIPLE CKS
      *  053195 BAH WAS NOT UPDATING DS-DLRVCHGS(CX-DLRVNO) AND
      *             ALWAYS REDUCED DS-DLRVPRIN(1) FOR "DR"
      *  103096 BLV FIX SAFECK (USE COPY MEMBER)
      *  031197 JTG - CORRECTED DTFILE UPDATE.
      *             - LOGIC WAS INSURING THAT DT-SEQ WAS = TO CX-DTSEQ
      *               AND BATCH CHECK RELEASE HAD TO SET CX-DTSEQ TO ZERO,
      *               SINCE ONE CX-REC COULD REFLECT MANY DT-REC'S,
      *               CAUSING A MISMATCH AND SKIPPED UPDATE OF DTFILE.
      *             - ADDED UPDATE LOGIC COPY MEMBER 'CKPRINTUPD'
      *  010197 JTG - INTEGRATED A90-LONP3T.C INTO A10-LONPM2.C
      *  MJD 032098 ADDED GETENV(W) FOR USE IN LIBLP/POSTDATE
      * GLB 011002  REWORK FOR NEW DEALER CHECK MODS / COPY MEMBERS
      *             LIBLP/CHECKTRANS REPLACES LIBLP/CKPRINTUPD
      * GLB 011004  FIX BATCH PRINT BUGS
      * GLB 011010  FIX ALIGNMENT PRINT BUGS
      * GLB 011018  CHANGE VOUCHER PRINT OPEN/CLOSE OF PR-FILE, DT4-FILE
      * BAH 020228  MESS WITH AUTO CHECK NUMBERING AND PRINTING ANY SYSTEM
      *             CHECK FROM SCREEN 7 (DOCUMENT PRINT), LORI/REGWAN PL#354
      *             WAS AUTOMATICALLY CREATING CHECK # 1 EVERYTIME !!
      * BAH 020305  WAS NOT ASKING FOR NEXT CHECK NUMBER ON MULTIPLE CHECKS
      *             FROM SCREEN 7 (DOCUMENT PRINT)....IT MUST !!
      * BAH 030306 CHECK NO WAS WIPING OUT FREE TYPE ON THE VOUCHER, AND
      *            THE WRITE-GL-INFO WAS REALLY BAD !! DUPLICATE PRINTED
      *            SOME AND LEFT OTHERS OUT...
      * BAH 040112  DO NOT TELL THEM TO INSERT A CHECK IF DOING HARD COPY OF
      *             BANK DEPOSIT OR BANK WITHDRAWAL....
      * BAH 040113  (BLV) FIX VOUCHER PRINT TO VERIFY THAT DEALER# & BATCH#
      *             OF DT'S MATCH THOSE FIELDS IN CX...WDT WAS HAVING
      *             DETAIL FROM DT'S FROM THE SAME CK# FROM YEARS AGO
      *             PRINTING INCORRECTLY ON CURRENT VOUCHERS!
      * BAH 040114 MOVED OPEN-CX1-FILE AND REMOVED CLOSE-CX1-FILE
      *            2 PEOPLE WERE ABLE TO PRINT THE SAME CHECK.
      *            1 WAS IN DOC PRINT, ENTER NEXT CHECK, WALKED AWAY TO
      *            ANOTHER SCREEN, WENT TO EXACT SAME PLACE, BUT ENTERED
      *            THE CHECK# AND SHOULD HAVE GOTTEN CX LOCKED, BUT DIDNT,
      *            IT ALLOWED IT TO PRINT, THEN WENT BACK TO OTHER TERMINAL
      *            AND WAS FORCED TO ENTER A CHECK # AGAIN, AND IT PRINTED
      *            AGAIN. SO, CX1 FILE NEEDS TO STAY LOCKED, DO NOT CLOSE
      * BAH 051215 BATCH PRINT WAS PRINTING CHECKS IN RESCISSION, PL# 525
      * BLF 060719 ADD PA-FILE & GPENV FOR UPDATE OF YTD PAID AMT IN
      *            PAFILE IN CHECKTRANS, DEVELOPMENT PR# 2633
      * BAH 110707 NEW LAYOUT, WORLD PR# 758
      * BAH 111107 REDUCE STUB GLDESC TO 7 AND INCREAST STUB GLAMTS TO 9
      *  CS 120222 ADDED NEW PAYEE TYPE 'ID' & 'IW' LOGIC TO SKIP PRINTING
      *            OF CHECK & CAUSE A DELETE OF THE 'CX'  WORLD PR#790
      *  CS 160204 REMOVED GIFILE SAVED VERSION LONP3T.160204
      * BLM 160512 REMOVE LPSETDSW, LPSETDS, PD0003
      * BAH 160818 MESS WITH LOAN-KEY, LN-ACCTNO AND CX-REF!!
      *            ALL BECAUSE WE CHANGED THE ACCTNO TO BE 8, THIS PROGRAM
      *            USED LN-SEQNO TO PASS THE CHECK TYPE, SO I CHANGED ALL THE
      *            LN-ACCTNO USAGE TO NOW USE LOAN-KEY
      *
      * KEC 2017.0119 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED FD-SMARTPORT TO FD-OPTION [88 LEVELS].
      *  CS 171127 CHANGED MOVE OF TRANSDATE/CKDATE/WKDATE TO CONVERT FROM
      *            YYYYMMDD TO MMDDYY
      * KEC 2017.0515 {PR#01242} WORLD <A15> [KEVIN MULLEN]
      *          ADDED NEW ROUTINES FOR LASER (PCL) CHECK VERSION.
      *          HAD TO ADD THE B2FILE AND LOGIC TO ACCESS THE B2-REC
      *          (PERFORM MISC-LOAD-B2-REC.) SO TO GET THE
      *          DOT MATRIX/LASER FLAG (B2-OPTION-PST-LASER = "Y" ).
      * AMM 2018.0116 {PR#01242} WORLD <A30> [KEVIN MULLEN]
      *          FINISHED ADDING THE PCL LASER ROUTINES FROM PR#1242; B2
      *          FIELDS ARE NOW IN THE BR FILE (DIFFERENT FROM A15)
      * BAH 181029 GLOBAL DTFILE
      * BAH 181114 GLOBAL FMFILE
      * BAH 2019.0201 PCL LASER DATES WERE WRONG FORMAT, MUST MATCH A15
      * BAH 2019.0204 FIXED CX-ACCTNO TRUNCATING ON VOUCHER
      * BAH 2019.0312 MOVED EDIT-FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY 
      * AMM 2020.0212 ADJUSTED SPACING ON LASER CHECK PRINT LINE #1422
      * BAH 2020.1120 HAD A PERIOD IN A BAD SPOT, CAUSING GL DESCRIPTION TO
      *               TO PRINT ON ALL 5 LINES.
      * BAH 2021.0305 DO NOT ASK TO INSERT A CHECK IF DOING DEBIT CARD, #1490
      * BAH 2022.0111 ADDED WK-PAYEE-NO, WAS 202020 IN NEW REPRINTED CHECKS
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

      ***************************************************
      *  INDEXED WORK FILE FOR CHECK REPRINT
      ***************************************************
       FD  WK-FILE,
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-NO           PIC 9(6).
               05  WK-SEQ          PIC 9(8).
           03  WK-BRNO             PIC 9(4)     COMP-3.
           03  WK-TYPE             PIC X(2).
           03  WK-DATE             PIC 9(8)     COMP-3.
           03  WK-AMT              PIC S9(7)V99 COMP-3.
           03  WK-PAYEE            PIC X(30).
           03  WK-REF              PIC 9(6)     COMP-3.
           03  WK-REASON           PIC X(30).
           03  WK-GLDIST           OCCURS 5.
               05  WK-GLNO         PIC 9(11)    COMP-3.
               05  WK-GLAMT        PIC S9(7)V99 COMP-3.
           03  WK-VOID             PIC X.
           03  WK-PAID             PIC X.
           03  WK-CLASS            PIC 9(3)     COMP-3.
           03  WK-DLNO             PIC 9(4)V99  COMP-3.
           03  WK-PGM              PIC XX.
           03  WK-VOID-DATE        PIC 9(8)     COMP-3.
           03  WK-ACCTNO           PIC 9(8)     COMP-3.
           03  WK-DLRVNO           PIC 9.
           03  WK-CLEARED-FG       PIC X.
           03  WK-SSNO             PIC 9(9)     COMP-3.
           03  WK-BATCHID          PIC 9(10)    COMP-3.
           03  WK-UNPAID           PIC X.
           03  WK-PAYEE-NO         PIC 9(6)     COMP-3.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./LP/LONP3T.CBL S35 02/27/24-11:43:25 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LP01CA.CPY".
       COPY "LIBLP/LP01CA_SQL.CPY".
       COPY "LIBLP/LPWSCA.CPY".
       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".
       COPY "LIBLP/LP01CX.CPY".
       COPY "LIBLP/LP01CX_SQL.CPY".
       COPY "LIBLP/LPWSCX.CPY".
       COPY "LIBLP/LP01DT.CPY".
       COPY "LIBLP/LP01DT_SQL.CPY".
       COPY "LIBLP/LPWSDT.CPY".
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LP01DS_SQL.CPY".
       COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LP01TY_SQL.CPY".
       COPY "LIBLP/LPWSTY.CPY".

       01  FREE-TEST.
           03  FREE1       PIC X(53).
           03  FREE2       PIC X(17).
       01  C-S-Z           PIC X(85).
       01  STATE           PIC XX.
       01  ZIP             PIC X(10).
       01  SCAN-TEST       PIC 9(7).
       01  SCAN-TESTX      REDEFINES SCAN-TEST.
           03  FILLER      PIC 9.
           03  TEST1       PIC 99.
           03  TEST2       PIC 9(4).

       01  COMPRESS-PRINT.
           03  FILLER                  PIC X VALUE X"1B".
           03  FILLER                  PIC X VALUE X"45".
       01  END-COMPRESS-PRINT.
           03  FILLER                  PIC X VALUE X"1B".
           03  FILLER                  PIC X VALUE X"26".
           03  FILLER                  PIC X VALUE X"6B".
           03  FILLER                  PIC X VALUE X"30".
           03  FILLER                  PIC X VALUE X"53".

       01  END-COMPRESS-LPI.
           03  FILLER                  PIC X VALUE X"1B".
           03  FILLER                  PIC X VALUE X"26".
           03  FILLER                  PIC X VALUE X"6C".
           03  FILLER                  PIC X VALUE X"36".
           03  FILLER                  PIC X VALUE X"44".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  NOTE: EJECT-PAGE-PCL DOES NOT WORK WITH DOT MATRIX
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *COPY "LIBGB/HEX_CMD.CPY".
       COPY "LIBLP/LASER_CMD.CPY".
       01  EJECT-PAGE-PCL.
           03  FILLER                  PIC X(1)  VALUE HEX-EC.
           03  FILLER                  PIC X(1)  VALUE HEX-UPPER-E.

       01  EJECT-PAGE-DOT-MATRIX.
           03  FILLER                  PIC X(1)  VALUE X"1B".
           03  FILLER                  PIC X(1)  VALUE X"26".
           03  FILLER                  PIC X(1)  VALUE X"6C".
           03  FILLER                  PIC X(1)  VALUE X"30".
           03  FILLER                  PIC X(1)  VALUE X"48".

       01  MANUAL-TRAY-PCL.
           03  FILLER                  PIC X(1)  VALUE X"1B".
           03  FILLER                  PIC X(1)  VALUE X"26".
           03  FILLER                  PIC X(1)  VALUE X"6C".
           03  FILLER                  PIC X(1)  VALUE X"32".
           03  FILLER                  PIC X(1)  VALUE X"48".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *      C H E C K   P R I N T   B U F F E R   L O N P 3 T
      *                 S T A N D A R D
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      **************************************
      *       CHECK VARIABLE TEXT          *
      **************************************
       01  TEXT1.
           03  FILLER        PIC X(11) VALUE " ".
           03  FILLER        PIC X(14) VALUE "DATE    REF".
           03  FILLER        PIC X(32) VALUE "REASON".
           03  FILLER        PIC X(6)  VALUE "AMOUNT".

      *********************************************
      *     C H E C K   B U F F E R               *
      *********************************************
       01  DETAIL-LINE             VALUE " ".
           03  PRINT-AREA          PIC X(80).

           03  STUB1 REDEFINES PRINT-AREA.
               05  FILLER          PIC X(7).
               05  D-CKDATE2       PIC 99/99/99.
               05  FILLER          PIC X.
               05  D-REF           PIC Z(6).
               05  FILLER          PIC X.
               05  D-REASON        PIC X(26).
               05  FILLER          PIC X.
               05  D-CKAMT2        PIC Z(7).99.
               05  FILLER          PIC X.
               05  D-CKTEXT        PIC X(8).
               05  D-CKTEXT-AMTX REDEFINES D-CKTEXT.
                   07  FILLER      PIC X.
                   07  D-CKTEXT-AMT PIC Z(6).
                   07  FILLER      PIC X.

           03  STUB2 REDEFINES PRINT-AREA.
               05  FILLER          PIC X(7).
               05  D-GL-NO         PIC Z(7).
               05  FILLER          PIC X.
               05  D-GL-DESC       PIC X(7).
               05  FILLER          PIC X.
               05  D-GL-AMT        PIC Z(7).99-.
               05  D-GL-NO2        PIC Z(7).
               05  FILLER          PIC X.
               05  D-GL-DESC2      PIC X(7).
               05  FILLER          PIC X.
               05  D-GL-AMT2       PIC Z(7).99-.

           03  STUB3 REDEFINES PRINT-AREA.
               05  FILLER          PIC X(7).
               05  D-FREE          PIC X(53).

           03  LINE1 REDEFINES PRINT-AREA.
               05  FILLER          PIC X(6).
               05  D-CHECKALP      PIC X(62).

           03  LINE2 REDEFINES PRINT-AREA.
               05  FILLER          PIC X(9).
               05  D-PAYEE         PIC X(31).
               05  FILLER          PIC X.
               05  D-CKDATE1       PIC 99/99/99.
               05  FILLER          PIC XX.
               05  D-CKAMT1        PIC $****,***.99.

           03  LINE3 REDEFINES PRINT-AREA.
               05  FILLER          PIC X(50).
               05  D-CK            PIC X(4).
               05  D-CKNO          PIC Z(6).

           03  LINE4 REDEFINES PRINT-AREA.
               05  FILLER          PIC X(3).
               05  D-MEMO          PIC X(6).
               05  D-FOR           PIC X(29).

      *========================================================[ PCL ]
      *   P C L - D E T A I L - L I N E S
      *===============================================================
       01  DETAIL-LINE-01            REDEFINES DETAIL-LINE.
           03  FILLER                PIC X(08).
           03  D01-CHECKALP          PIC X(62).

       01  DETAIL-LINE-02            REDEFINES DETAIL-LINE.
           03  FILLER                PIC X(12).
           03  D02-PAYEE             PIC X(40).
           03  FILLER                PIC X(01).
           03  D02-CKDATE1           PIC 99/99/99.
           03  FILLER                PIC X(04).
           03  D02-CKAMT1            PIC $****,***.99.

       01  DETAIL-LINE-03            REDEFINES DETAIL-LINE.
           03  FILLER                PIC X(03).
           03  D03-MEMO              PIC X(06).
           03  D03-FOR               PIC X(29).

       01  STUB-LINE-01              REDEFINES DETAIL-LINE.
           03  FILLER                PIC X(07).
           03  S01-CKDATE2           PIC 99/99/99.
           03  FILLER                PIC X(01).
           03  S01-REF               PIC Z(06).
           03  FILLER                PIC X(01).
           03  S01-REASON            PIC X(26).
           03  FILLER                PIC X(01).
           03  S01-CKAMT2            PIC Z(07).99.
           03  FILLER                PIC X(01).
           03  S01-CKTEXT            PIC X(08).
           03  FILLER                REDEFINES S01-CKTEXT.
               05  FILLER            PIC X(01).
               05  S01-CKTEXT-AMT    PIC Z(06).
               05  FILLER            PIC X(01).

       01  STUB-LINE-03              REDEFINES DETAIL-LINE.
           03  FILLER                PIC X(07).
           03  S03-FREE              PIC X(53).

       01  STUB-LINE-04              REDEFINES DETAIL-LINE.
           03  FILLER                PIC X(50).
           03  S04-CK                PIC X(04).
           03  S04-CKNO              PIC Z(06).

       01  VOUCHER-LINE-01           REDEFINES DETAIL-LINE.
           03  FILLER                PIC X(05).
           03  V01-PROMPT            PIC X(20).
           03  V01-TEXT              PIC X(30).
           03  V01-CKAMT             REDEFINES V01-TEXT
                                     PIC $****,***.99.
           03  V01-DATE              REDEFINES V01-TEXT
                                     PIC 99/99/9999.
           03  V01-NUMBER            REDEFINES V01-TEXT
                                     PIC ZZZZZZZZ9.
           03  FILLER                PIC X(01).
           03  V01-GL-NO             PIC Z(07).
           03  FILLER                PIC X(01).
           03  V01-GL-DESC           PIC X(16).

       COPY "LIBLP/CHECKTRANSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/SAFECKW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBLP/LPDSCLW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPM_DEF.CPY".
       COPY "LIBLP/CHCKI_DEF.CPY".
       COPY "LIBLP/BCHKI_DEF.CPY".
       COPY "LIBLP/LONPM_WKS.CPY".
       COPY "LIBLP/CHCKI_WKS.CPY".
       COPY "LIBLP/BCHKI_WKS.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPIPBUF.CPY".
       COPY "LIBGB/GETPRW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPM_SCN.CPY".
       COPY "LIBLP/CHCKI_SCN.CPY".
       COPY "LIBLP/BCHKI_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME PROG-BUF IP-BUF PRU-PATH EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-FM2-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-DT4-FILE.
           PERFORM CLOSE-DT2-FILE.
           PERFORM CLOSE-DT1-FILE.
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-CX1-FILE.
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           IF NEW-PROG = "M0"
              MOVE "LONPM" TO VDU-FORM-NAME
           ELSE
              IF LPLOAN-CALLED-FROM = "BCHKI0"
                 MOVE "BCHKI" TO VDU-FORM-NAME
              ELSE
                 MOVE "CHCKI" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-ROUTINE.

      * THIS OPEN-CX1-FILE MUST STAY HERE, FILE NEEDS TO STAY LOCKED
      * DO NOT ADD ANY CLOSES OF CX IN THIS PROGRAM EITHER.
           PERFORM OPEN-CX1-FILE.

      *-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
      *  BATCH-PRINT   ( BATCH-PR = "Y") CALLED FROM BATCH FORM PRINT
      *  DIRECT-PRINT  ( BATCH-PR = " " OR "N")  CALLED FROM NEW LOANS
      *                                          OR CHECK WRITING
      *  INITIAL-PRINT (IP-PRFG   = "P")
      *  REPRINT       (IP-PRFG   = "R")
      *-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

           IF (DIRECT-PRINT)
              GO TO DETERMINE-LOANKEY.

           MOVE 0 TO LOAN-KEY.
           MOVE SPACES TO FM-REC LPWSFM-EDIT-FM-INITKEY.

           IF (INITIAL-PRINT)
              MOVE FM-PATH-OWNBR TO FM-BRNO
              MOVE IP-FMNO       TO FM-FMNO LPWSFM-EDIT-FM-CKFMNO
              MOVE SPACES        TO FM-ZIP
              PERFORM START-FM2-FILE
              GO TO NEXT-CHECK-REQUEST.


      **************************************************
      *    R  E  P  R  I  N  T      L  O  O  P
      *    IF REPRINTING IN BATCH MODE, READ WORK FILE
      **************************************************
       NEXT-CHECK-REPRINT.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           MOVE WK-BRNO      TO CX-BRNO.
           MOVE WK-NO        TO CX-CKNO.
           MOVE 0            TO CX-CKNO-NEW.
           MOVE WK-VOID      TO CX-VOID.
      *    MOVE WK-VOID-DATE TO CX-VOID-DATE.
           MOVE WK-CLASS     TO CX-CLASS.
           MOVE WK-DLNO      TO CX-DLNO.
           MOVE WK-DLRVNO    TO CX-DLRVNO.
           MOVE WK-DATE      TO CX-DATE.
           MOVE WK-PGM       TO CX-PGM.
           MOVE WK-PAID      TO CX-PAID.
           MOVE WK-TYPE      TO CX-TYPE.
           MOVE WK-AMT       TO CX-AMT.
           MOVE WK-PAYEE     TO CX-PAYEE.
           MOVE WK-PAYEE-NO  TO CX-PAYEE-NO.
           MOVE WK-REF       TO CX-REF.
           MOVE WK-ACCTNO    TO  CX-ACCTNO.
           MOVE WK-REASON    TO CX-REASON.
           MOVE WK-SSNO      TO CX-SSNO.
           MOVE WK-BATCHID   TO CX-BATCHID.
           MOVE WK-UNPAID    TO CX-DT-UNPAID.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              MOVE WK-GLNO(SUB) TO CX-GLNO(SUB)
              COMPUTE CX-GLAMT(SUB) = WK-GLAMT(SUB)
           END-PERFORM.

           MOVE "V"          TO CX-PAID.

           MOVE CX-ACCTNO TO LOAN-KEY.
           MOVE CX-TYPE   TO CHECK-PAYEE.

           GO TO CX-TEMP-FOUND.

      *---------------------------------------------------------
       DETERMINE-LOANKEY.
      *    FIND CHECK TYPE TO LOCATE CX-REC.
      *    LONPI0 CREATES 1 SYSTEM CHECK AT A TIME, (CASH DISBURSE)
      *    2ONPI0 CREATES 1 SYSTEM CHECK AT A TIME, (CHECK WRITING)
      *    AND PASSES TT-CKWRITTEN SUBSCRIPT AND CX-SEQNO IN LOAN-KEY.
      *    CX-REF IS FORCED TO CX-SEQNO, PERMITTING COMPARE.

      *  SCAN FLAG ADDED BECAUSE CX-REF ISNT ALWAYS A
      *    99 + SEQNO WHEN SCANNING THE FILE
      *---------------------------------------------------------
           MOVE LOAN-KEY TO LN-ACCTNO.


      *---------------------------------------------------------
      *  CALLED BY CASH/CHECK WRITING PROGRAM IF NEW-PROG = I0
      *---------------------------------------------------------
           IF NEW-PROG = "I0"
              IF CXFILE-SCAN-FLAG = "Y"
                 MOVE LOAN-KEY-SEQNO TO SCAN-TEST
                 IF TEST1 NOT = 99
                    MOVE P-CODE(LOAN-KEY-TYPE) TO LPWSFM-EDIT-FM-TYPE
                    MOVE 0                     TO LPWSFM-EDIT-FM-SEQNO
                    GO TO SET-FM-KEY.

           IF NEW-PROG = "I0"
              MOVE LOAN-KEY-SEQNO        TO LPWSFM-EDIT-FM-SEQNO
              MOVE P-CODE(LOAN-KEY-TYPE) TO LPWSFM-EDIT-FM-TYPE
              GO TO SET-FM-KEY.


      *---------------------------------------------------------
      *  CALLED BY PROGRAM OTHER THAN CASH/CHECK WRITING
      *---------------------------------------------------------
       READ-LOAN-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 9
              MOVE "LOAN RECORD DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.
           PERFORM CLOSE-LN1-FILE.

           IF LN-SALEFIN = "Y"
              MOVE "DL" TO LPWSFM-EDIT-FM-TYPE
           ELSE
              MOVE "LN" TO LPWSFM-EDIT-FM-TYPE.

           MOVE 0 TO LPWSFM-EDIT-FM-SEQNO.

      *---------------------------------------------------------
      *  COMMON ROUTINE FOR DIRECT PRINTS
      *---------------------------------------------------------
       SET-FM-KEY.

           MOVE FM-PATH-OWNBR           TO FM-BRNO.
           MOVE HOLD-BRNO               TO LPWSFM-EDIT-FM-BRNO.
           MOVE LPWSFM-EDIT-FM-TYPE     TO CHECK-PAYEE.
           MOVE 0                       TO LPWSFM-EDIT-FM-CKFMNO.
           MOVE LPWSFM-EDIT-FM-INITKEY  TO FM-INITKEY.
           MOVE 0                       TO FM-FMNO.

           PERFORM START-FM1-FILE.

      **************************************************
      *    I N I T I A L    P R I N T    L O O P
      *    IF INITIAL PRINT IN BATCH MODE OR DIRECT
      **************************************************
       NEXT-CHECK-REQUEST.
           IF (BATCH-PRINT)
              PERFORM READ-FM2-FILE-NEXT
           ELSE
              PERFORM READ-FM1-FILE-NEXT.
           IF IO-FG = 9 OR (FM-BRNO NOT = FM-PATH-OWNBR)
              GO TO END-ROUTINE.

      *    UNLOCK FM-FILE.
           IF FM-PRINT = "Y"
              GO TO NEXT-CHECK-REQUEST.

           MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY.

      * IF IN BATCH MODE, PRINT ALL UNPRINTED REQUESTS
      * THAT MATCH THE CHECK PROGRAM FORM NO.
           IF (BATCH-PRINT)
              IF FM-FMNO NOT = IP-FMNO
                 GO TO END-ROUTINE
              END-IF
              MOVE LPWSFM-EDIT-FM-TYPE TO CHECK-PAYEE
           ELSE
              IF LPWSFM-EDIT-FM-BRNO NOT = HOLD-BRNO OR
                 LPWSFM-EDIT-FM-TYPE NOT = CHECK-PAYEE
                 GO TO END-ROUTINE.

           MOVE LPWSFM-EDIT-FM-BRNO  TO CX-BRNO.
           MOVE LPWSFM-EDIT-FM-TYPE  TO CX-TYPE.
           MOVE LPWSFM-EDIT-FM-SEQNO TO CX-SEQNO.
           PERFORM READ-CX1-FILE.
           IF IO-FG = 9
              GO TO NEXT-CHECK-REQUEST.

           IF CX-RESCISSION = "Y"
              GO TO NEXT-CHECK-REQUEST.

           IF (BATCH-PRINT)
              MOVE CX-REF TO LN-ACCTNO.
      * THIS IS A MESS SINCE WE CHANGE LN-ACCTNO! AND LOAN-KEY GOT EXPANDED
      * TO 9 FOR APPS, SO I AM NOW USING LOAN-KEY TO FIND THE CORRECT
      * CX RECORD, BUT DIDNT WANT TO MESS WITH THE MOVE FOR BATCH PRINT
      * FOR A "MS" MISC CHECK, CX-REF = 990007 99(SEQ) SEQ CHANGES EACH TIME
      * LN-ACCTNO AT THIS POINT IS TOO BIG, USED TO BE 6 SO ONLY HELD 990007
      * BUT NOW IT HOLD 99000703, HAS THE CHECK TYPE ON THE END, THATS WHY I
      * AM USING LOAN-KEY-SEQNO, IT IS ONLY THE FIRST 6 FROM LOAN-KEY
           IF CX-REF = LN-ACCTNO OR
               CX-REF = LOAN-KEY-SEQNO
              GO TO CX-TEMP-FOUND.
           GO TO NEXT-CHECK-REQUEST.

       CX-TEMP-FOUND.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           PERFORM CLOSE-BR-FILE.

           PERFORM VERIFY-CHECK-PAYEE.

      *-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
      *     C H E C K     N O .      A S S I G N
      *-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

      * IF PRINTING/UPDATING A CASH/BANK TRANSACTION,
      *    NO CHECK NO.
      *-----------------------------------------------
           IF (BANKTR) OR (INTERDEP) OR (INTERWITH)
              GO TO OK-TO-PRINT.
           IF (CASHTR)
              GO TO OK-TO-PRINT.



      * SYSTEM CHECK # ALREADY ENTERED IN 2ONPI0
      *     (CHECK WRITING, SO DIRECT PRINT)
      *-----------------------------------------------
           IF (DIRECT-PRINT)
              IF NEW-PROG = "I0"
                 MOVE CX-CKNO TO CHECK-NO
                 IF CX-CKNO-NEW NOT = 0
                    MOVE CX-CKNO-NEW TO CHECK-NO
                 END-IF
                 GO TO OK-TO-PRINT.

      * IF BATCH PRINTING, ENTER NEXT CHECK NO. FOR
      *     THE FIRST CHECK ONLY, WHETHER PRINT/REPRINT
      *-----------------------------------------------
           IF FIRST-CHECK = "Y"
              IF (BATCH-PRINT)
                 GO TO ENTER-CHECK-NO.

      * IF AUTOMATICALLY ASSIGNING NEXT CHECK NO.,
      *    WE NEED TO GET THE NEXT BRANCH CHECK#, THEN FALL THRU AND
      *      DISPLAY IT !!
      *  OTHERWISE, INCREMENT CHECK# BY 1 ONLY FOR NEW BATCH CHECK PRINT
      *-----------------------------------------------
           IF BR-AUTO-CHECK = "Y"
              MOVE BR-NEXT-CHECK TO CHECK-NO
           ELSE
              IF (BATCH-PRINT)
                 ADD 1 TO CHECK-NO
                 GO TO TEST-CHECK-NO.

      *---------------------------------------------------------
       ENTER-CHECK-NO.
      *---------------------------------------------------------
           IF (DIRECT-PRINT)
              MOVE "                           NEXT CHECK #:" TO MESS
           ELSE
              MOVE "F7-MENU, ^-BACK, ENTER NEXT CHECK#:" TO MESS.
           PERFORM SEND-LEGEND.
           IF BR-AUTO-CHECK = "Y"
              MOVE "SN N060CKNO." TO VDU
              MOVE BR-NEXT-CHECK TO VDUNUM0
              PERFORM SCREENIO
              GO TO TEST-CHECK-NO.
           MOVE "EN N060CKNO." TO VDU.
           PERFORM SCREENIO.
           IF (BATCH-PRINT)
              IF FIRST-CHECK = "Y"
                 IF VDUSTATUS  = "1>"
                    MOVE "X" TO IP-AOK
                    GO TO END-ROUTINE.
           IF (BATCH-PRINT)
              IF FIRST-CHECK = "Y"
                 IF VDUSTATUS  = "1U"
                    MOVE "U" TO IP-AOK
                    GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00" GO TO ENTER-CHECK-NO.
           INSPECT VDUNUM0 REPLACING ALL " " BY "0".
           IF VDUNUM0 = 0 GO TO ENTER-CHECK-NO.
           MOVE VDUNUM0 TO CHECK-NO.

      *---------------------------------------------------------
       TEST-CHECK-NO.
      *---------------------------------------------------------
           MOVE BRANCH   TO CK-BRNO.
           MOVE CHECK-NO TO CK-NO.
           PERFORM OPEN-CK1-FILE.
           PERFORM READ-CK1-FILE.
           PERFORM CLOSE-CK1-FILE.
           IF IO-FG = 0
              MOVE "CHECK ALREADY EXISTS!" TO MESS
              PERFORM SEND-MESS
              IF BR-AUTO-CHECK = "Y"
                 MOVE "CORRECT NEXT CHECK NUMBER IN BRANCH RECORD!"
                 TO MESS
                 PERFORM SEND-MESS
      * GOES HERE, SO YOU CAN GO CHANGE THE BR REC AND KEEP GOING !!
                 GO TO CX-TEMP-FOUND
              ELSE
                 GO TO ENTER-CHECK-NO.

           IF (DIRECT-PRINT)
              MOVE CHECK-NO TO CX-CKNO
              GO TO OK-TO-PRINT.

      *---------------------------------------------------------
      *  B A T C H    P R I N T   A C C E P T / A L I G N
      *---------------------------------------------------------
           IF FIRST-CHECK = "N"
              GO TO OK-TO-PRINT.
       ACCEPT-PRINT.
           MOVE "ENTER 'Y' TO BATCH PRINT, 'A' ALIGNMENT TEST"
              TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A000ACCEPT." TO VDU
           PERFORM SCREENIO
           IF VDUSTATUS  = "1>"
              MOVE "X" TO IP-AOK
              GO TO END-ROUTINE.
           IF VDUSTATUS  = "1U" GO TO ENTER-CHECK-NO.
           IF VDUSTATUS NOT = "00" GO TO ACCEPT-PRINT.

           IF VDUBUF = "Y" GO TO OK-TO-PRINT.
           IF VDUBUF = "N" GO TO ENTER-CHECK-NO.
           IF VDUBUF NOT = "A" GO TO ACCEPT-PRINT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CHECK IF WORLD BRANCH IS USING DOT MATRIX OR LASER (PCL)
      * NOTE: THIS USES PCL *NOT* PST; USES PCL IN THIS PROGRAM.
      * GLB 10/10/01 OPEN/CLOSE PRINTER IN ALIGNMENT ROUTINE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF ( BR-OPTION-PST-LASER = "Y" )
              PERFORM PCL-ALIGNMENT-PRINT
           ELSE
              PERFORM ALIGNMENT-PRINT.

           PERFORM CREATE-CK-ALIGNMENT.
           PERFORM UPDATE-TC-ALIGNMENT.
           ADD 1 TO CK-NO.
           MOVE CK-NO TO CHECK-NO.
           MOVE "SN N060CKNO." TO VDU.
           MOVE CHECK-NO TO VDUNUM0.
           PERFORM SCREENIO.

           IF BR-AUTO-CHECK NOT = "Y"
              GO TO TEST-CHECK-NO.
           PERFORM OPEN-BR-FILE.
           MOVE BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           MOVE CHECK-NO TO BR-NEXT-CHECK.
           PERFORM REWRITE-BR-FILE.
           PERFORM CLOSE-BR-FILE.

           GO TO ENTER-CHECK-NO.

      *---------------------------------------------------------
      *    P R I N T     C H E C K    /    H A R D    C O P Y
      *---------------------------------------------------------
       OK-TO-PRINT.
      * GLB 10/10/01
           PERFORM TEST-CUTSHEET.

           MOVE "PRINT IN PROGRESS................." TO MESS.
           PERFORM SEND-LEGEND.


      * GLB 10/04/01
           IF (BATCH-PRINT)
              IF (REPRINT)
                 MOVE CHECK-NO TO CX-CKNO-NEW
                 PERFORM SETUP-REPRINT-FREE
              ELSE
                 MOVE CHECK-NO TO CX-CKNO.

           PERFORM SET-PRINTER-FIELDS.

           PERFORM OPEN-PR-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CHECK IF WORLD BRANCH IS USING DOT MATRIX OR LASER (PCL)
      * NOTE: THIS USES PCL *NOT* PST; USES PCL IN THIS PROGRAM.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF ( BR-OPTION-PST-LASER = "Y" )
              PERFORM PCL-CHECK-PRINT
           ELSE
              PERFORM CHECK-PRINT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "N" TO FIRST-CHECK.
           PERFORM CLOSE-PR-FILE.

           PERFORM PRU-RESET.

           PERFORM UPDATE-FILES-CHECKTRANS.

           IF (BANKTR) OR (CASHTR) OR (INTERDEP) OR (INTERWITH)
              GO TO SKIP-BR-CHECK-UPDATE.

           IF BR-AUTO-CHECK = "Y"
              OR (BATCH-PRINT)
                 PERFORM OPEN-BR-FILE
                 PERFORM READ-BR-FILE
                 IF IO-FG NOT = 0
                    GO TO IO-ERROR
                 ELSE
                    ADD 1 TO BR-NEXT-CHECK
                    PERFORM REWRITE-BR-FILE
                    PERFORM CLOSE-BR-FILE.

           IF (BATCH-PRINT)
              IF (REPRINT)
                 GO TO NEXT-CHECK-REPRINT.

       SKIP-BR-CHECK-UPDATE.
           PERFORM READ-FM1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE "Y" TO FM-PRINT.
           PERFORM REWRITE-FM1-FILE.

           IF (BATCH-PRINT)
              GO TO NEXT-CHECK-REQUEST.

           IF NEW-PROG = "I0"
      * ONE CHECK ENVIRONMENT
      * "R" ALLOWS RESET
              MOVE "R" TO IP-AOK
              GO TO END-ROUTINE.

           GO TO NEXT-CHECK-REQUEST.

      ***********************************
       VERIFY-CHECK-PAYEE SECTION.
      ***********************************
           MOVE 0 TO PAYEE-SUB.
       NEXT-PAYEE-VERIFY.
           ADD 1 TO PAYEE-SUB.
           IF P-CODE(PAYEE-SUB) = " "
              GO TO NEXT-PAYEE-VERIFY.
           IF P-CODE(PAYEE-SUB) = CHECK-PAYEE
              GO TO EXIT-VERIFY-PAYEE.
           IF PAYEE-SUB < PAYEE-MAX
              GO TO NEXT-PAYEE-VERIFY.
           MOVE 99 TO PAYEE-SUB.
       EXIT-VERIFY-PAYEE.
           EXIT.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
       WRITE-DET-LINE.
           IF LN-FEED > 15
              MOVE SPACES TO PR-REC
              MOVE LN-FEED TO SAVE-LN-FEED
              MOVE 15 TO LN-FEED
              PERFORM WRITE-PR-REC
              SUBTRACT 15 FROM SAVE-LN-FEED GIVING LN-FEED
              GO TO WRITE-DET-LINE.
           IF CONT-PRINT NOT = "1"
              MOVE DETAIL-LINE TO PR-REC-CUTSHEET
           ELSE
             MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.


      *********************************
       INITIALIZATION SECTION.
      *********************************
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE SPACES TO ERRCD.

           MOVE BRANCH TO HOLD-BRNO.

      * CLEAR THE "P" TO PRINT
           MOVE "S  A000SEL." TO VDU.
           PERFORM SCREENIO.

           PERFORM OPEN-FD1-FILE.
           MOVE IP-FMNO TO FD-FMNO.
           PERFORM READ-FD1-FILE.
           IF IO-FG NOT = 0
              MOVE "N" TO FD-OPTION.
           PERFORM CLOSE-FD1-FILE.

           IF (DIRECT-PRINT) OR (INITIAL-PRINT)
              PERFORM OPEN-FM1-FILE
              GO TO CONTINUE-INITIALIZE.

           PERFORM BUILD-REPRINT-FILE.
           IF HOLD-SEQ = 0
              MOVE "NO CHECKS TO REPRINT IN THIS CHECK RANGE "
              TO MESS
               PERFORM SEND-MESS.

           PERFORM OPEN-WK-FILE.
           MOVE 0 TO WK-NO.
           PERFORM START-WK-FILE.

       CONTINUE-INITIALIZE.

           GO TO EXIT-INITIALIZE.

       END-INITIALIZE.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.


      *********************************
       BUILD-REPRINT-FILE SECTION.
      *********************************
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.

           PERFORM OPEN-WK-FILE.
           MOVE 0 TO HOLD-SEQ.
           PERFORM OPEN-CK1-FILE.

           MOVE BRANCH TO CK-BRNO
                          QCK1-WBEG-BRNO
                          QCK1-WEND-BRNO.
           MOVE CK1    TO CK-NO
                          QCK1-WBEG-CKNO.
           MOVE CK2    TO QCK1-WEND-CKNO.

           PERFORM START-CK1-FILE.
       NEXT-REPRINT-BUILD.
           PERFORM READ-CK1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-BUILD-REPRINT.

           IF CK-BRNO NOT = BRANCH
              GO TO END-BUILD-REPRINT.

           IF CK-NO > CK2
              GO TO END-BUILD-REPRINT.
           IF CK-VOID = "Y"
              GO TO NEXT-REPRINT-BUILD.
      * GLB 10/10/01 ALREADY REPRINTED/VOIDED
           IF CK-VOID = "R"
              GO TO NEXT-REPRINT-BUILD.
      * ONLY REPRINT SYSTEM CHECKS ?
      *    IF CK-PAID NOT = "S"
      *       GO TO NEXT-REPRINT-BUILD.

           MOVE CK-BRNO      TO WK-BRNO.
           MOVE CK-NO        TO WK-NO.
           MOVE CK-VOID      TO WK-VOID.
           MOVE CK-VOID-DATE TO WK-VOID-DATE.
           MOVE CK-CLASS     TO WK-CLASS.
           MOVE CK-NO        TO WK-NO.
           MOVE CK-DLNO      TO WK-DLNO.
           MOVE CK-DLRVNO    TO WK-DLRVNO.
           MOVE CK-DATE      TO WK-DATE.
           MOVE CK-PGM       TO WK-PGM.
           MOVE CK-PAID      TO WK-PAID.
           MOVE CK-TYPE      TO WK-TYPE.
           MOVE CK-AMT       TO WK-AMT.
           MOVE CK-PAYEE     TO WK-PAYEE.
           MOVE CK-PAYEE-NO  TO WK-PAYEE-NO.
           MOVE CK-REF       TO WK-REF.
           MOVE CK-ACCTNO    TO WK-ACCTNO.
           MOVE CK-REASON    TO WK-REASON.
           MOVE CK-SSNO      TO WK-SSNO.
           MOVE CK-BATCHID   TO WK-BATCHID.
           MOVE CK-UNPAID    TO WK-UNPAID.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              MOVE CK-GLNO(SUB) TO WK-GLNO(SUB)
              COMPUTE WK-GLAMT(SUB) = CK-GLAMT(SUB)
           END-PERFORM.

           ADD 1         TO HOLD-SEQ.
           MOVE HOLD-SEQ TO WK-SEQ.

           PERFORM WRITE-WK-FILE.
           IF IO-BAD
              GO TO IO-ERROR.
           GO TO NEXT-REPRINT-BUILD.
       END-BUILD-REPRINT.
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-WK-FILE.


      *********************************
       CHECK-PRINT SECTION.
      *********************************
           IF ( FD-OPTION-SMARTPORT )
              STRING "LONP3T",FD-FMNO DELIMITED BY "  " INTO
               DETAIL-LINE
              MOVE 0 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-CKDATE2.
           IF BR-ALLOW-SYS-AS-EFFDATE = "Y"
              MOVE CX-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-CKDATE2.
           

           MOVE CX-REF TO D-REF.
           IF CX-ACCTNO NOT = 0
              MOVE CX-ACCTNO TO D-REF.
           MOVE CX-REASON TO D-REASON.
           MOVE CX-AMT TO D-CKAMT2.
      * IF THEY TYPED FREE TYPE, YOU HAVE TO PRINT CHECK NO OUTSIDE
      * OF THE BOX, THERE IS NO OTHER ROOM
           IF CX-FREE1 NOT = SPACES
              IF CHECK-NO NOT = 0
                 MOVE "CHECK NO" TO D-CKTEXT.
           MOVE 0 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF CX-FREE1 NOT = SPACES
              IF CHECK-NO NOT = 0
                 MOVE CHECK-NO TO D-CKTEXT-AMT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-GL-INFO.

           MOVE CX-FREE1 TO FREE-TEST.
           MOVE FREE1 TO D-FREE.
           IF CX-FREE1 = SPACES AND CHECK-NO NOT = 0
              MOVE "CK# " TO D-CK
              MOVE CHECK-NO TO D-CKNO.
           PERFORM WRITE-DETAIL-LINE.

           IF FREE2 NOT = SPACES
              STRING FREE2," ",CX-FREE2 DELIMITED BY "  " INTO
                D-FREE
           ELSE
              MOVE CX-FREE2 TO D-FREE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE CX-AMT TO NUM-DOLLARS.
           PERFORM SAFECK.
           MOVE ALP-LINE TO D-CHECKALP.
           MOVE 10 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE CX-PAYEE TO D-PAYEE.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-CKDATE1.
 
           MOVE CX-AMT TO D-CKAMT1.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MEMO: " TO D-MEMO.
           MOVE CX-REASON TO D-FOR.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 7 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( FD-OPTION-SMARTPORT )
              MOVE LASER-EJECT-PAGE TO PR-REC
              PERFORM WRITE-PR-REC.

      **********************************
       WRITE-GL-INFO SECTION.
      **********************************
           PERFORM OPEN-GL1-FILE.

           MOVE 1 TO SUB.
       NEXT-GL-INFO.
           IF CX-GLNO(SUB) = 0
              GO TO CHECK-GL2.

           MOVE CX-GLNO(SUB) TO GL-BR-ACCOUNT.
           PERFORM READ-GL1-FILE.
           IF IO-FG NOT = 0
              MOVE "************" TO GL-DESCRIPTION
              MOVE CX-GLNO(SUB)   TO GL-BR-ACCOUNT.
           MOVE GL-ACCOUNT TO D-GL-NO.
           MOVE GL-DESCRIPTION TO D-GL-DESC.
           MOVE CX-GLAMT(SUB) TO D-GL-AMT.

       CHECK-GL2.
           ADD 1 TO SUB.
           IF SUB > 5
              GO TO PRINT-GL-LINE.
           IF CX-GLNO(SUB) = 0
              GO TO PRINT-GL-LINE.

           MOVE CX-GLNO(SUB) TO GL-BR-ACCOUNT.
           PERFORM READ-GL1-FILE.
           IF IO-FG NOT = 0
              MOVE "************" TO GL-DESCRIPTION
              MOVE CX-GLNO(SUB)   TO GL-BR-ACCOUNT.
           MOVE GL-ACCOUNT TO D-GL-NO2.
           MOVE GL-DESCRIPTION TO D-GL-DESC2.
           MOVE CX-GLAMT(SUB) TO D-GL-AMT2.

       PRINT-GL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           ADD 1 TO SUB.
           IF SUB NOT > 5
              MOVE 1 TO LN-FEED
              GO TO NEXT-GL-INFO.

       EXIT-WRITE-GL-INFO.
           PERFORM CLOSE-GL1-FILE.


      **********************************
       ALIGNMENT-PRINT SECTION.
      **********************************

           PERFORM TEST-CUTSHEET.

           MOVE "ALIGNMENT IN PROGRESS................." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM SET-PRINTER-FIELDS.
           PERFORM OPEN-PR-FILE.

           IF ( FD-OPTION-SMARTPORT )
              STRING "LONP3T",FD-FMNO DELIMITED BY "  " INTO
               DETAIL-LINE
              MOVE 0 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.
      
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY            TO D-CKDATE2.
           MOVE ALL "9" TO D-REF.
           MOVE ALL "X" TO D-REASON.
           MOVE ALL "9" TO D-CKAMT2.
           MOVE 0 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 1 TO SUB.
           MOVE 2 TO LN-FEED.
       NEXT-GL-INFO2.
           MOVE ALL "9" TO D-GL-NO.
           MOVE "G/L DESCRIPTION" TO D-GL-DESC.
           MOVE ALL "9" TO D-GL-AMT.
           MOVE ALL "9" TO D-GL-NO2.
           MOVE "G/L DESCRIPTION" TO D-GL-DESC2.
           MOVE ALL "9" TO D-GL-AMT2.
           PERFORM WRITE-DETAIL-LINE.
           ADD 1 TO SUB.
           IF SUB < 4
              MOVE 1 TO LN-FEED
              GO TO NEXT-GL-INFO2.

           MOVE "                   V  O  I  D " TO D-FREE.
           MOVE CHECK-NO               TO D-CKNO.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ALIGNMENT PRINT TEST" TO D-FREE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "P A Y E E" TO D-PAYEE.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-CKDATE1.

           MOVE ALL "9" TO D-CKAMT1.
           MOVE 7 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "*** VOID CHECK ***" TO D-FOR.
           MOVE 5 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 7 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


      *    MOVE "9" TO NUM-DOLLARS.
      *    PERFORM SAFECK.
      *    MOVE "$ 0 DOLLARS AND 00/100 CENTS    V  O  I D" TO ALP-LINE.
      *    MOVE ALP-LINE TO D-CHECKALP.
      *    MOVE 2 TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.


      *    MOVE ALL "X" TO C-S-Z.
      *    MOVE ALL "X" TO ZIP.
      *    MOVE ALL "X" TO STATE.
      *    PERFORM CAT-CSZ.
      *    MOVE C-S-Z TO D-PAYEE.
      *    MOVE 1 TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.

      *    MOVE TEXT1 TO D-MSG.
      *    MOVE 7 TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.

      *ALIGN-BOTTOM.
      *    MOVE 12 TO LN-FEED.
      *ALIGN-TOP.
      *    IF CONT-PRINT = "1"
      *       COMPUTE LN-FEED = LN-FEED + 8.
      *    MOVE SPACES TO PRINT-AREA.
      *    PERFORM WRITE-DETAIL-LINE.

           IF ( FD-OPTION-SMARTPORT )
              MOVE LASER-EJECT-PAGE TO PR-REC
              PERFORM WRITE-PR-REC.

           PERFORM CLOSE-PR-FILE.
           PERFORM PRU-RESET.

      **********************************
       PCL-ALIGNMENT-PRINT SECTION.
      **********************************

           PERFORM TEST-CUTSHEET.

           MOVE "ALIGNMENT IN PROGRESS................." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-PR-FILE.

      *- - - - - - - - - - - - - - - - - - -{ VOUCHER LINE: 01 }
           MOVE "DATE:"                 TO V01-PROMPT.
           MOVE 99999999                TO V01-TEXT.
           MOVE 9999999                 TO V01-GL-NO.
           MOVE ALL "X"                 TO V01-GL-DESC.
           MOVE ZEROES                  TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - -{ VOUCHER LINE: 02 }
           MOVE "PAY TO THE ORDER:"     TO V01-PROMPT.
           MOVE ALL "X"                 TO V01-TEXT.
           MOVE 9999999                 TO V01-GL-NO.
           MOVE ALL "X"                 TO V01-GL-DESC.
           MOVE 02                      TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - -{ VOUCHER LINE: 03 }
           MOVE "AMOUNT:"               TO V01-PROMPT.
           MOVE 99999.99                TO V01-CKAMT.
           MOVE 9999999                 TO V01-GL-NO.
           MOVE ALL "X"                 TO V01-GL-DESC.
           MOVE 02                      TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - -{ VOUCHER LINE: 04 }
           MOVE "CHECK NUMBER:"         TO V01-PROMPT.
           MOVE 99999999                TO V01-TEXT.
           MOVE 9999999                 TO V01-GL-NO.
           MOVE ALL "X"                 TO V01-GL-DESC.
           MOVE 02                      TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - -{ VOUCHER LINE: 05 }
           MOVE "MEMO:"                 TO V01-PROMPT.
           MOVE ALL "X"                 TO V01-TEXT.
           MOVE 9999999                 TO V01-GL-NO.
           MOVE ALL "X"                 TO V01-GL-DESC.
           MOVE 02                      TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - -{ STUB LINE: 01  }
           MOVE 99999999                TO S01-CKDATE2.

           IF ( BR-ALLOW-SYS-AS-EFFDATE = "Y" )
              MOVE 88888888             TO S01-CKDATE2.

           MOVE "*** REFERENCE ***"     TO S01-REF.

           IF ( CX-ACCTNO NOT = ZEROES )
              MOVE CX-ACCTNO            TO S01-REF.

           MOVE CX-REASON               TO S01-REASON.
           MOVE CX-AMT                  TO S01-CKAMT2.

      * IF THEY TYPED FREE TYPE, YOU HAVE TO PRINT CHECK NO OUTSIDE
      * OF THE BOX, THERE IS NO OTHER ROOM

           IF ( CX-FREE1 NOT = SPACES )
              IF ( CHECK-NO NOT = ZEROES )
                 MOVE "CHECK NO"         TO S01-CKTEXT.

           MOVE 0 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - -{ STUB LINE: 02  }
           IF ( CX-FREE1 NOT = SPACES )
              IF ( CHECK-NO NOT = ZEROES )
                 MOVE CHECK-NO           TO S01-CKTEXT-AMT.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - -{ STUB LINE: 03  }
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-GL-INFO.

      *- - - - - - - - - - - - - - - - - - - -{ STUB LINE: 04  }
           MOVE CX-FREE1                 TO FREE-TEST.
           MOVE FREE1                    TO S03-FREE.

           IF ( CX-FREE1     = SPACES ) AND
              ( CHECK-NO NOT = ZEROES )
              MOVE "CK# "                TO S04-CK
              MOVE CHECK-NO              TO S04-CKNO.

           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - -{ STUB LINE: 05  }
           IF ( FREE2 NOT = SPACES )
              STRING FREE2," ",CX-FREE2
                    DELIMITED BY "  " INTO S03-FREE
           ELSE
              MOVE CX-FREE2              TO S03-FREE.

           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 1 TO SUB.
           MOVE 2 TO LN-FEED.

      *- - - - - - - - - - - - - - - -{ CHECK/DETAIL LINE: 01  }
           MOVE "                   V O I D " TO S03-FREE.
           MOVE CHECK-NO                 TO S04-CKNO.
           PERFORM WRITE-DETAIL-LINE.
 
           MOVE "ALIGNMENT PRINT TEST"   TO S03-FREE.
           PERFORM WRITE-DETAIL-LINE.
          
           MOVE "P A Y E E"              TO D02-PAYEE.
           MOVE TRANS-DATE               TO D02-CKDATE1.
           MOVE ALL "9"                  TO D02-CKAMT1.
           MOVE 7 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "*** VOID CHECK ***"     TO D03-FOR.
           MOVE 5 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 7 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *KC  MOVE EJECT-PAGE-PCL TO PR-REC.
      *KC  PERFORM WRITE-PR-REC.

           PERFORM CLOSE-PR-FILE.
           PERFORM PRU-RESET.

       PCL-ALIGNMENT-PRINT-EXIT.
           EXIT.

      **********************************
       PCL-CHECK-PRINT SECTION.
      **********************************

      *- - - - - - - - - - - - - - - - - -{ PRINT TO MANUAL TRAY}
           MOVE MANUAL-TRAY-PCL TO PR-REC.
           MOVE 00 TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - -{ VOUCHER LINE: 01 }
           MOVE "DATE:"                 TO V01-PROMPT.
           IF ( BR-ALLOW-SYS-AS-EFFDATE = "Y" )
              MOVE CX-DATE              TO DATE-YYYYMMDD
           ELSE
              MOVE TRANS-DATE           TO DATE-YYYYMMDD.

           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY           TO V01-DATE.

           MOVE 01                      TO SUB.
 
           IF ( CX-GLNO(SUB) NOT = ZEROES )
              PERFORM MISC-LOAD-GL-REC
              MOVE GL-ACCOUNT           TO V01-GL-NO
              MOVE GL-DESCRIPTION       TO V01-GL-DESC.

           MOVE 03                      TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - -{ VOUCHER LINE: 02 }
           MOVE "PAY TO THE ORDER:"     TO V01-PROMPT.
           MOVE CX-PAYEE                TO V01-TEXT.

           MOVE 02                      TO SUB.

           IF ( CX-GLNO(SUB) NOT = ZEROES )
              PERFORM MISC-LOAD-GL-REC
              MOVE GL-ACCOUNT           TO V01-GL-NO
              MOVE GL-DESCRIPTION       TO V01-GL-DESC.

           MOVE 02                      TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - -{ VOUCHER LINE: 03 }
           MOVE "AMOUNT:"               TO V01-PROMPT.
           MOVE CX-AMT                  TO V01-CKAMT.

           MOVE 03                      TO SUB.

           IF ( CX-GLNO(SUB) NOT = ZEROES )
              PERFORM MISC-LOAD-GL-REC
              MOVE GL-ACCOUNT           TO V01-GL-NO
              MOVE GL-DESCRIPTION       TO V01-GL-DESC.

           MOVE 02                      TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - -{ VOUCHER LINE: 04 }
           MOVE "CHECK NUMBER:"         TO V01-PROMPT.
           MOVE CHECK-NO                TO V01-NUMBER.

           MOVE 04                      TO SUB.

           IF ( CX-GLNO(SUB) NOT = ZEROES )
              PERFORM MISC-LOAD-GL-REC
              MOVE GL-ACCOUNT           TO V01-GL-NO
              MOVE GL-DESCRIPTION       TO V01-GL-DESC.

           MOVE 02                      TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - -{ VOUCHER LINE: 05 }
           MOVE "MEMO:"                 TO V01-PROMPT.
           MOVE CX-REASON               TO V01-TEXT.

           MOVE 05                      TO SUB.

           IF ( CX-GLNO(SUB) NOT = ZEROES )
              PERFORM MISC-LOAD-GL-REC
              MOVE GL-ACCOUNT           TO V01-GL-NO
              MOVE GL-DESCRIPTION       TO V01-GL-DESC.

           MOVE 02                      TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - -{ STUB LINE: 01  }
           MOVE TRANS-DATE              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO S01-CKDATE2.

           IF ( BR-ALLOW-SYS-AS-EFFDATE = "Y" )
              MOVE CX-DATE              TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY           TO S01-CKDATE2.

           MOVE CX-REF                  TO S01-REF.

           IF ( CX-ACCTNO NOT = ZEROES )
              MOVE CX-ACCTNO            TO S01-REF.

           MOVE CX-REASON               TO S01-REASON.
           MOVE CX-AMT                  TO S01-CKAMT2.

      * IF THEY TYPED FREE TYPE, YOU HAVE TO PRINT CHECK NO OUTSIDE
      * OF THE BOX, THERE IS NO OTHER ROOM

           IF ( CX-FREE1 NOT = SPACES )
              IF ( CHECK-NO NOT = ZEROES )
                 MOVE "CHECK NO"         TO S01-CKTEXT.

           MOVE 13 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - -{ STUB LINE: 02  }
           IF ( CX-FREE1 NOT = SPACES )
              IF ( CHECK-NO NOT = ZEROES )
                 MOVE CHECK-NO           TO S01-CKTEXT-AMT.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - -{ STUB LINE: 03  }
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-GL-INFO.

      *- - - - - - - - - - - - - - - - - - - -{ STUB LINE: 04  }
           MOVE CX-FREE1                 TO FREE-TEST.
           MOVE FREE1                    TO S03-FREE.

           IF ( CX-FREE1     = SPACES ) AND
              ( CHECK-NO NOT = ZEROES )
              MOVE "CK# "                TO S04-CK
              MOVE CHECK-NO              TO S04-CKNO.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - -{ STUB LINE: 05  }
           IF ( FREE2 NOT = SPACES )
              STRING FREE2," ",CX-FREE2
                    DELIMITED BY "  " INTO S03-FREE
           ELSE
              MOVE CX-FREE2              TO S03-FREE.

           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - -{ CHECK/DETAIL LINE: 01  }
           MOVE CX-AMT                   TO NUM-DOLLARS.
           PERFORM SAFECK.
           MOVE ALP-LINE                 TO D01-CHECKALP.

           MOVE 18 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
          
      *- - - - - - - - - - - - - - - -{ CHECK/DETAIL LINE: 02  }
           MOVE CX-PAYEE                 TO D02-PAYEE.
           MOVE TRANS-DATE               TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D02-CKDATE1.
           MOVE CX-AMT                   TO D02-CKAMT1.

           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - -{ CHECK/DETAIL LINE: 03  }
           MOVE "MEMO: "                 TO D03-MEMO.
           MOVE CX-REASON                TO D03-FOR.

           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PCL-CHECK-PRINT-EXIT.
           EXIT.

       COPY "LIBLP/CHECKTRANS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBLP/LPDTCL.CPY".
       COPY "LIBLP/LPDSCL.CPY".
       COPY "LIBLP/LPTRCCL.CPY".
       COPY "LIBLP/LPTYCL.CPY".
       COPY "LIBLP/LPCKCL.CPY".

      * WORLD DIDNT WANT THE 3 LEADING ASTERICKS AND NEEDED WORD
      * DOLLARS ON THE END
      ********************************************
       SAFECK SECTION.
      ********************************************
       ALP-1.
           MOVE 0 TO NEXT-3.
           MOVE SPACES TO ALP-LINE.
           MOVE ND-CENTS TO D-CENTS.
           MOVE "NO DOLLARS" TO ALP-DOLLARS.

           IF ND-M > 0
              MOVE ALP-1-DOLS(ND-M) TO ALP-DOLLARS
              PERFORM MOVE-ALP-DOLS
              MOVE "MILLION" TO ALP-DOLLARS
              PERFORM MOVE-ALP-DOLS.
       ALP-2.
           IF ND-B > 0
              MOVE ALP-1-DOLS(ND-B) TO ALP-DOLLARS
              PERFORM MOVE-ALP-DOLS
              MOVE "HUNDRED" TO ALP-DOLLARS
              PERFORM MOVE-ALP-DOLS.
           IF ND-E > 0
              IF ND-E < 20
                 MOVE ALP-1-DOLS(ND-E) TO ALP-DOLLARS
                 PERFORM MOVE-ALP-DOLS
              ELSE
                 COMPUTE ND-SUB = ND-E / 10 - 1
                 MOVE ALP-2-DOLS (ND-SUB) TO ALP-DOLLARS
                 PERFORM MOVE-ALP-DOLS
                 IF ND-D > 0
                    MOVE ALP-1-DOLS(ND-D) TO ALP-DOLLARS
                    PERFORM MOVE-ALP-DOLS.
           IF ND-A > 0
              IF NEXT-3 = 0
                 MOVE "THOUSAND" TO ALP-DOLLARS
                 PERFORM MOVE-ALP-DOLS.
           IF NEXT-3 = 0
              MOVE 1 TO NEXT-3
              MOVE ND-F TO ND-A
              GO TO ALP-2.
           IF ALP-DOLLARS = "NO DOLLARS"
              PERFORM MOVE-ALP-DOLS.
           MOVE CENTS-MSG TO ALP-DOLLARS.
           PERFORM MOVE-ALP-DOLS.
           MOVE "DOLLARS" TO ALP-DOLLARS.
           PERFORM MOVE-ALP-DOLS.
           GO TO EXIT-SAFECK.
       MOVE-ALP-DOLS.
           INSPECT ALP-LINE REPLACING FIRST "  " BY " +".
           INSPECT ALP-LINE
              REPLACING FIRST "+         " BY ALP-DOLLARS.
       EXIT-SAFECK.
           EXIT.

      ****************************************
       SETUP-LINK-INFO SECTION.
      ****************************************
           MOVE LONPM-HELP--FILE TO HELP-LINK-FILE.
           MOVE LONPM-HELP--DIR TO HELP-LINK-DIR.

      ****************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPM_EVA.CPY".
              COPY "LIBLP/CHCKI_EVA.CPY".
              COPY "LIBLP/BCHKI_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       SET-PRINTER-FIELDS.
           MOVE 0 TO Q-POS.
           MOVE 0 TO C-POS.
           MOVE LONPM-PRU-POS TO P-POS.
           MOVE "N" TO Q-BUF.
           MOVE 00 TO P-BUF.
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".

       MISC-LOAD-GL-REC.
           MOVE CX-GLNO(SUB)             TO GL-BR-ACCOUNT.
           PERFORM OPEN-GL1-FILE.
           PERFORM READ-GL1-FILE.
           PERFORM CLOSE-GL1-FILE.

           IF ( IO-BAD )
              MOVE "*************"       TO GL-DESCRIPTION
              MOVE CX-GLNO(SUB)          TO GL-BR-ACCOUNT.

       SETUP-REPRINT-FREE.
           MOVE CX-CKNO TO CHECK-NO2.
           MOVE "VOID CK#&&&&&&; ISSUE #$$$$$$" TO CX-FREE1.
           INSPECT CX-FREE1 REPLACING FIRST "&&&&&&" BY CHECK-NO2.
           MOVE CX-CKNO-NEW TO CHECK-NO2.
           INSPECT CX-FREE1 REPLACING FIRST "$$$$$$" BY CHECK-NO2.

       SEND-LEGEND.
           MOVE "S  A400MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       CAT-CSZ.
           INSPECT C-S-Z REPLACING FIRST "      " BY ", ++ +".
           INSPECT C-S-Z REPLACING FIRST "++" BY STATE.
           INSPECT C-S-Z REPLACING FIRST "+         " BY ZIP.
       TEST-CUTSHEET.
      * DO NOT TELL THEM TO INSERT A CHECK IF DOING HARD COPY OF
      * BANK DEPOSIT OR BANK WITHDRAWAL....
           IF NOT (CHECK-PAYEE = "CA" OR "BK" OR "ID" OR "IW" OR "DB")
              IF CONT-PRINT NOT = "1"
                 MOVE "INSERT NEXT CHECK, PRESS <ENTER> TO PRINT"
                          TO MESS
                 PERFORM SEND-MESS.

       COPY "LIBGB/GETPR.CPY".

      ***************************************
       IO-ROUTINE SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPDTGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPCXGS_SQL.CPY".
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRIN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBLP/LPCA1IN.CPY".
       COPY "LIBLP/LPCK1IN.CPY".
       COPY "LIBLP/LPCX1IN.CPY".
       COPY "LIBLP/LPDT1IN.CPY".
       COPY "LIBLP/LPDT2IN.CPY".
       COPY "LIBLP/LPDT4IN.CPY".
       COPY "LIBLP/LPDS1IN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPFM2RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPTRC1IN.CPY".
       COPY "LIBLP/LPTY1IN.CPY".

       COPY "LIBGB/GBWKIN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ***************************************
       END-ROUTINE SECTION.
      ***************************************
           PERFORM CLOSE-FILES.

           MOVE "SN A060CKNO." TO VDU
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

      * PRU-RESET IS PERFORMED UP ABOVE SO MESSAGES GO TO SCREEN
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
