      * COPYMEMBER: LIBLP/LPFDTR
      ******************************************************************
      *
      * DESCRIPTION:  DAILY TRANSACTION FILE
      *
      * FILE INFO  :  LOCAL ED/TR###  (####=MMYY)     
      *
      *               SUBKEY MAY BE ANY FIELD WHICH
      *               REDEFINES THIS AREA OF THE RECORD
      *
      * LOW-VALUES IS USED TO INITIALIZE THE KEY BECAUSE OF THE SEVERAL
      * DIFFERENT KEY REDEFINITIONS. USE CAUTION WHEN DOING A START OF
      * OF THE TR-FILE AND MAKE SURE YOU HAVE ALL NECESSARY FIELDS FILLED
      * IN PROPERLY.
      *
      *=================================================================
      * REV:
      *  CS 160919 CHANGED TR-SUBKEY TO 17
      *            SAVED LPFDTR.160920
      *  CS 160920 INSTALLED UPDATED VERSION
      * BAH 180125 REDUCED FILLER AND FILE SIZE PER KM
      ******************************************************************
       FD  OTR-FILE
           LABEL RECORDS ARE STANDARD.
      ******************************************************************
      *
      * DESCRIPTION:  DAILY TRANSACTION FILE
      *
      * FILE INFO  :  LOCAL ED/TR###  (####=MMYY)             
      *
      *               SUBKEY MAY BE ANY FIELD WHICH
      *               REDEFINES THIS AREA OF THE RECORD
      *
      *=================================================================
      *
      * ****NOTE****  ****NOTE****  ****NOTE****  ****NOTE****
      *
      * THE TR-REC IS REDEFINED BY MULTIPLE VERSIONS BASED ON THE 
      * TR1-CODE.  EACH VERSION USES THE SAME LAYOUT OF TR1-KEY WITH
      * THE TR1-SUBKEY DIFFERENT FOR THE VERSION.
      *
      * LAYOUT   TR1-CODE  DESCRIPTION
      * =======  ========  ===========================================
      * LPFDTRB  B         BRANCH (BULK) TRANSFERS
      * LPFDTRC  C         CHECKS/CASH
      * LPFDTRD  D         DEPOSIT
      * LPFDTRE  E         REVOLVING SALES FINANCE NEW INPUT
      * LPFDTRF  E***      REVOLVING DIRECT LOAN TRANSACTION RECORD
      * LPFDTRI  I         INSURANCE CLAIMS
      * LPFDTRJ  J         CLASS TRANSFERS (WITH IN ONE BRANCH)
      * LPFDTRK  K         CONVERSION
      * LPFDTRL  L         DIRECT LOAN
      * LPFDTRP  P         PAYMENTS
      *          F***      PAYMENTS WITH POOLID
      *          G***      PAYMENTS FOR SET POOL (TRANS CODE = SP)
      * LPFDTRS  S         SALES FINANCE NEW INPUT
      * LPFDTRW  W         CASH BALANCING RECORD
      * LPFDTRX  X         CLASS TRANSFERS
      *
      ******************************************************************
       78  OTR-SIZE                         VALUE 950.
       01  OTR-REC.
      *        KEY IS 27 BYTES TOTAL:
           03  OTR1-KEY.
               05  OTR1-DAY                 PIC 9(2).     
               05  OTR1-BRNO                PIC 9(4).                  
               05  OTR1-CODE                PIC X.
                   88  OTR1-VALID-CD-B          VALUE "B".
                   88  OTR1-VALID-CD-C          VALUE "C".
                   88  OTR1-VALID-CD-D          VALUE "D".
                   88  OTR1-VALID-CD-G          VALUE "G".
                   88  OTR1-VALID-CD-I          VALUE "I".
                   88  OTR1-VALID-CD-J          VALUE "J".
                   88  OTR1-VALID-CD-K          VALUE "K".
                   88  OTR1-VALID-CD-L          VALUE "L".
                   88  OTR1-VALID-CD-P          VALUE "P".
                   88  OTR1-VALID-CD-S          VALUE "S".
                   88  OTR1-VALID-CD-X          VALUE "X".
                   88  OTR1-VALID-CD-ALL        VALUE "B" "C" "D" "G"
                                                     "J" "K" "L" "P"
                                                     "S" "X" "I".
               05  OTR1-CLASS               PIC 999.                 
               05  OTR1-SUBKEY              PIC X(17).

      *>>XDD DATE=YYYYMMDD
           03  OTR-LTOUCH-DATE              PIC 9(8)   COMP-3.
           03  FILLER                      PIC X(918).

      ******************************************************************
       COPY "LIBUP/A30SRC/LPFDTRB.CPY" REPLACING LEADING "TB" BY "OTB".
       COPY "LIBUP/A30SRC/LPFDTRC.CPY" REPLACING LEADING "TC" BY "OTC".
       COPY "LIBUP/A30SRC/LPFDTRD.CPY" REPLACING LEADING "TD" BY "OTD".
       COPY "LIBUP/A30SRC/LPFDTRI.CPY" REPLACING LEADING "TI" BY "OTI".
       COPY "LIBUP/A30SRC/LPFDTRJ.CPY" REPLACING LEADING "TJ" BY "OTJ".
       COPY "LIBUP/A30SRC/LPFDTRK.CPY" REPLACING LEADING "TK" BY "OTK".
       COPY "LIBUP/A30SRC/LPFDTRL.CPY" REPLACING LEADING "TL" BY "OTL".
       COPY "LIBUP/A30SRC/LPFDTRP.CPY" REPLACING LEADING "TP" BY "OTP".
       COPY "LIBUP/A30SRC/LPFDTRS.CPY" REPLACING LEADING "TS" BY "OTS".
       COPY "LIBUP/A30SRC/LPFDTRW.CPY" REPLACING LEADING "TW" BY "OTW".
       COPY "LIBUP/A30SRC/LPFDTRX.CPY" REPLACING LEADING "TX" BY "OTX".
