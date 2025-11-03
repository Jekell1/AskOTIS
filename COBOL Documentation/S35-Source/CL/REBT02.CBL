      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.       REBT02.
       DATE-WRITTEN.   11/22/95.
      ******************************************************************
      *                      (CL)
      *    DESCRIPTION:  NEVADA, MERCURY PERSONNAL PROPERTY REFUND
      *                  CALLED FROM REBATE ROUTINE
      *                  SP-RBEARLY(6) = 13
      *
      *=================================================================
      * REV:
      * KEC 2015-0608 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED THE USE OF 'INDEXED BY' ON OCCURS
      *         FOR USE WITH MICROFOCUS.
      *
      * KEC 2023-0127 FOR S35, COMMENTED OUT THE USE OF GBFILE AND
      *               SQL COPY MEMBERS; NOT NEEDED
      *
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
      /
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)CL/REBT02.CBL S35 01/27/23-12:28:26 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
      *COPY "LIBGB/GB01GB.CPY".
      *COPY "LIBGB/GB01GB_SQL.CPY".
      *COPY "LIBGB/GBWSGB.CPY".

       01  ERRCD               PIC X VALUE " ".
       01  MONTHLY-PREM        PIC 9(6)V9(12).
       01  EARNED-PREM         PIC 9(6)V99.
       01  DAYS                PIC 9(5).
       01  ELAPSED             PIC 9(4)V9(14).
       01  ELAPSEX             REDEFINES ELAPSED.
           03  ELAP-YEARS      PIC 9(4).
           03  ELAP-FRAC       PIC V9(14).

      *COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".

       01  REBATE-TABLE.
           03  FILLER          PIC X(7) VALUE "000 000".
           03  FILLER          PIC X(7) VALUE "001 005".
           03  FILLER          PIC X(7) VALUE "002 006".
           03  FILLER          PIC X(7) VALUE "004 007".
           03  FILLER          PIC X(7) VALUE "006 008".
           03  FILLER          PIC X(7) VALUE "008 009".
           03  FILLER          PIC X(7) VALUE "010 010".
           03  FILLER          PIC X(7) VALUE "012 011".
           03  FILLER          PIC X(7) VALUE "014 012".
           03  FILLER          PIC X(7) VALUE "016 013".
           03  FILLER          PIC X(7) VALUE "018 014".
           03  FILLER          PIC X(7) VALUE "020 015".
           03  FILLER          PIC X(7) VALUE "022 016".
           03  FILLER          PIC X(7) VALUE "025 017".
           03  FILLER          PIC X(7) VALUE "029 018".
           03  FILLER          PIC X(7) VALUE "032 019".
           03  FILLER          PIC X(7) VALUE "036 020".
           03  FILLER          PIC X(7) VALUE "040 021".
           03  FILLER          PIC X(7) VALUE "043 022".
           03  FILLER          PIC X(7) VALUE "047 023".
           03  FILLER          PIC X(7) VALUE "051 024".
           03  FILLER          PIC X(7) VALUE "054 025".
           03  FILLER          PIC X(7) VALUE "058 026".
           03  FILLER          PIC X(7) VALUE "062 027".
           03  FILLER          PIC X(7) VALUE "065 028".
           03  FILLER          PIC X(7) VALUE "069 029".
           03  FILLER          PIC X(7) VALUE "073 030".
           03  FILLER          PIC X(7) VALUE "076 031".
           03  FILLER          PIC X(7) VALUE "080 032".
           03  FILLER          PIC X(7) VALUE "083 033".
           03  FILLER          PIC X(7) VALUE "087 034".
           03  FILLER          PIC X(7) VALUE "091 035".
           03  FILLER          PIC X(7) VALUE "094 036".
           03  FILLER          PIC X(7) VALUE "098 037".
           03  FILLER          PIC X(7) VALUE "102 038".
           03  FILLER          PIC X(7) VALUE "105 039".
           03  FILLER          PIC X(7) VALUE "109 040".
           03  FILLER          PIC X(7) VALUE "113 041".
           03  FILLER          PIC X(7) VALUE "116 042".
           03  FILLER          PIC X(7) VALUE "120 043".
           03  FILLER          PIC X(7) VALUE "124 044".
           03  FILLER          PIC X(7) VALUE "127 045".
           03  FILLER          PIC X(7) VALUE "131 046".
           03  FILLER          PIC X(7) VALUE "135 047".
           03  FILLER          PIC X(7) VALUE "138 048".
           03  FILLER          PIC X(7) VALUE "142 049".
           03  FILLER          PIC X(7) VALUE "146 050".
           03  FILLER          PIC X(7) VALUE "149 051".
           03  FILLER          PIC X(7) VALUE "153 052".
           03  FILLER          PIC X(7) VALUE "156 053".
           03  FILLER          PIC X(7) VALUE "160 054".
           03  FILLER          PIC X(7) VALUE "164 055".
           03  FILLER          PIC X(7) VALUE "167 056".
           03  FILLER          PIC X(7) VALUE "171 057".
           03  FILLER          PIC X(7) VALUE "175 058".
           03  FILLER          PIC X(7) VALUE "178 059".
           03  FILLER          PIC X(7) VALUE "182 060".
           03  FILLER          PIC X(7) VALUE "187 061".
           03  FILLER          PIC X(7) VALUE "191 062".
           03  FILLER          PIC X(7) VALUE "196 063".
           03  FILLER          PIC X(7) VALUE "200 064".
           03  FILLER          PIC X(7) VALUE "205 065".
           03  FILLER          PIC X(7) VALUE "209 066".
           03  FILLER          PIC X(7) VALUE "214 067".
           03  FILLER          PIC X(7) VALUE "218 068".
           03  FILLER          PIC X(7) VALUE "223 069".
           03  FILLER          PIC X(7) VALUE "228 070".
           03  FILLER          PIC X(7) VALUE "232 071".
           03  FILLER          PIC X(7) VALUE "237 072".
           03  FILLER          PIC X(7) VALUE "241 073".
           03  FILLER          PIC X(7) VALUE "246 074".
           03  FILLER          PIC X(7) VALUE "250 075".
           03  FILLER          PIC X(7) VALUE "255 076".
           03  FILLER          PIC X(7) VALUE "260 077".
           03  FILLER          PIC X(7) VALUE "264 078".
           03  FILLER          PIC X(7) VALUE "269 079".
           03  FILLER          PIC X(7) VALUE "273 080".
           03  FILLER          PIC X(7) VALUE "278 081".
           03  FILLER          PIC X(7) VALUE "282 082".
           03  FILLER          PIC X(7) VALUE "287 083".
           03  FILLER          PIC X(7) VALUE "291 084".
           03  FILLER          PIC X(7) VALUE "296 085".
           03  FILLER          PIC X(7) VALUE "301 086".
           03  FILLER          PIC X(7) VALUE "305 087".
           03  FILLER          PIC X(7) VALUE "310 088".
           03  FILLER          PIC X(7) VALUE "314 089".
           03  FILLER          PIC X(7) VALUE "319 090".
           03  FILLER          PIC X(7) VALUE "323 091".
           03  FILLER          PIC X(7) VALUE "328 092".
           03  FILLER          PIC X(7) VALUE "332 093".
           03  FILLER          PIC X(7) VALUE "337 094".
           03  FILLER          PIC X(7) VALUE "342 095".
           03  FILLER          PIC X(7) VALUE "346 096".
           03  FILLER          PIC X(7) VALUE "351 097".
           03  FILLER          PIC X(7) VALUE "355 098".
           03  FILLER          PIC X(7) VALUE "360 099".
           03  FILLER          PIC X(7) VALUE "365 100".
       01  RB-TABLE           REDEFINES REBATE-TABLE.
           03  RB-TBL          OCCURS 97.
               05  RB-DAYS     PIC 999.
               05  FILLER      PIC X.
               05  RB-PER      PIC 999.

       78  RBIX-MAX            VALUE 97.
       77  RBIX                PIC 9(02).

      ****************************************
      *     LINKAGE PROGRAM SECTION:
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       01  REB-COMMON.
           03  REB-COMMON-RTNRB       PIC X.
           03  REB-COMMON-REBATE      PIC S9(7)V99 COMP-3.
           03  REB-COMMON-AREA        PIC X(40).

      * USED FOR SP-RBFRMLA = 'T', TABLE REFUND FORMULA:
           03  REB-T02                REDEFINES REB-COMMON-AREA.
               05  T02-ELAPSED-DAYS   PIC S9(5).
               05  T02-INSTERM        PIC 9(5).
               05  FILLER             PIC X(30).
       COPY "LIBLP/LP01LN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME REB-COMMON LN-REC EXIT-PATHNAME.

      *************************************************
       MAIN-MODULE SECTION.
      *************************************************

      *    PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
      D    STOP MESS.

           MOVE 0 TO REB-COMMON-REBATE.
           COMPUTE MONTHLY-PREM ROUNDED =
                      LN-INSPREM(3) / T02-INSTERM.
           COMPUTE ELAPSED ROUNDED = T02-ELAPSED-DAYS / 365.
           COMPUTE DAYS ROUNDED = ELAP-FRAC * 365.
           COMPUTE EARNED-PREM ROUNDED =
                             ELAP-YEARS * 12 * MONTHLY-PREM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  KEC 2015.0608 REMOVED 'INDEXED BY' FOR 'OCCURS'
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SET RBIX TO 1.
      *    SEARCH RB-TBL
      *        WHEN RB-DAYS(RBIX) NOT < DAYS
      *             COMPUTE EARNED-PREM ROUNDED =
      *                 EARNED-PREM
      *                    + RB-PER(RBIX) / 100 * 12 * MONTHLY-PREM.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE STAT---BAD                 TO STAT.

           PERFORM VARYING RBIX FROM 1 BY 1
                     UNTIL RBIX > RBIX-MAX
                        OR STAT-GOOD
                   IF ( RB-DAYS(RBIX) NOT < DAYS )
                      COMPUTE EARNED-PREM ROUNDED = EARNED-PREM
                                                  + RB-PER(RBIX)
                                                  / 100
                                                  * 12
                                                  * MONTHLY-PREM
                      MOVE STAT---GOOD     TO STAT
                   END-IF
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF EARNED-PREM NOT > LN-INSPREM(3)
              COMPUTE REB-COMMON-REBATE =
                              LN-INSPREM(3) - EARNED-PREM.
           GO TO END-PROGRAM.

       COPY "LIBGB/DATER.CPY".

      *************************************************
       IO-ROUTINE SECTION.
      *************************************************
      *COPY "LIBGB/CONNECT_SQL.CPY".
      *COPY "LIBGB/DECLARE_SQL.CPY".
      *COPY "LIBGB/GBGBGS_SQL.CPY".

       END-PROGRAM SECTION.
       ENDIT.
      *    PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
