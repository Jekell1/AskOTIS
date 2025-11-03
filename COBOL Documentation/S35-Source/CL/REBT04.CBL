      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.       REBT04.
       DATE-WRITTEN.   09/30/02.
      *********************************************************************
      *                      (CL)
      *    DESCRIPTION:  SHORT RATE FORMULA SP-RBEARLY = 24 (PP)    TFC #96
      *                  CALLED FROM REBATE ROUTINE
      *                  SP-RBEARLY(6) = 24
      * REV:
      * JTG 020930 **NEW**
      *
      * KEC 2015-0608 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED THE USE OF 'INDEXED BY' ON OCCURS
      *         FOR USE WITH MICROFOCUS.
      *
      * KEC 2023-0127 FOR S35, COMMENTED OUT THE USE OF GBFILE AND
      *               SQL COPY MEMBERS; NOT NEEDED
      *
      *********************************************************************
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
           "@(#)CL/REBT04.CBL S35 01/27/23-12:40:57 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
      *COPY "LIBGB/GB01GB.CPY".
      *COPY "LIBGB/GB01GB_SQL.CPY".
      *COPY "LIBGB/GBWSGB.CPY".
       01  REBATE-AMT          PIC S9(7)V99.
       01  ERRCD               PIC X VALUE " ".

      *COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".

       01  REBATE-TABLE.
           03  FILLER          PIC X(7) VALUE "000 000".
           03  FILLER          PIC X(7) VALUE "001 095".
           03  FILLER          PIC X(7) VALUE "002 094".
           03  FILLER          PIC X(7) VALUE "004 092".
           03  FILLER          PIC X(7) VALUE "007 090".
           03  FILLER          PIC X(7) VALUE "010 088".
           03  FILLER          PIC X(7) VALUE "013 086".
           03  FILLER          PIC X(7) VALUE "017 084".
           03  FILLER          PIC X(7) VALUE "021 082".
           03  FILLER          PIC X(7) VALUE "024 080".
           03  FILLER          PIC X(7) VALUE "029 078".
           03  FILLER          PIC X(7) VALUE "035 075".
           03  FILLER          PIC X(7) VALUE "043 072".
           03  FILLER          PIC X(7) VALUE "050 069".
           03  FILLER          PIC X(7) VALUE "057 066".
           03  FILLER          PIC X(7) VALUE "065 063".
           03  FILLER          PIC X(7) VALUE "072 060".
           03  FILLER          PIC X(7) VALUE "080 057".
           03  FILLER          PIC X(7) VALUE "087 054".
           03  FILLER          PIC X(7) VALUE "095 051".
           03  FILLER          PIC X(7) VALUE "103 048".
           03  FILLER          PIC X(7) VALUE "111 045".
           03  FILLER          PIC X(7) VALUE "119 042".
           03  FILLER          PIC X(7) VALUE "128 039".
           03  FILLER          PIC X(7) VALUE "137 036".
           03  FILLER          PIC X(7) VALUE "145 033".
           03  FILLER          PIC X(7) VALUE "155 030".
           03  FILLER          PIC X(7) VALUE "163 027".
           03  FILLER          PIC X(7) VALUE "173 024".
           03  FILLER          PIC X(7) VALUE "184 021".
           03  FILLER          PIC X(7) VALUE "196 018".
           03  FILLER          PIC X(7) VALUE "209 015".
           03  FILLER          PIC X(7) VALUE "226 013".
           03  FILLER          PIC X(7) VALUE "239 011".
           03  FILLER          PIC X(7) VALUE "252 009".
           03  FILLER          PIC X(7) VALUE "266 007".
           03  FILLER          PIC X(7) VALUE "280 005".
           03  FILLER          PIC X(7) VALUE "296 003".
           03  FILLER          PIC X(7) VALUE "318 001".
           03  FILLER          PIC X(7) VALUE "351 000".
           03  FILLER          PIC X(7) VALUE "999 000".
       01  RB-TABLE           REDEFINES REBATE-TABLE.
           03  RB-TBL          OCCURS 41.
               05  RB-DAYS     PIC 999.
               05  FILLER      PIC X.
               05  RB-PER      PIC 999.

       78  RBIX-MAX                       VALUE 41.
       77  RBIX                PIC 9(02)  VALUE ZEROES. 

      ****************************************
      *     LINKAGE PROGRAM SECTION:
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       01  REB-COMMON.
           03  REB-COMMON-RTNRB       PIC X.
           03  REB-COMMON-REBATE      PIC S9(7)V99 COMP-3.
           03  REB-COMMON-AREA        PIC X(40).

      * USED FOR SP-RBEARLY = '24', TABLE REFUND FORMULA:
           03  REB-T04               REDEFINES REB-COMMON-AREA.
               05  T04-ELAPSED-DAYS   PIC S9(5).
               05  FILLER             PIC X(35).
       COPY "LIBLP/LP01LN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME REB-COMMON LN-REC EXIT-PATHNAME.

      *****************************************************
       MAIN-MODULE SECTION.
      *****************************************************

      *    PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
       MAIN-ROUTINE.
      D    STOP MESS.
           MOVE 0 TO REB-COMMON-REBATE
                     REBATE-AMT.
           IF T04-ELAPSED-DAYS < 0
              MOVE 0 TO T04-ELAPSED-DAYS
           ELSE
           IF T04-ELAPSED-DAYS > 366
              MOVE 999 TO T04-ELAPSED-DAYS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  KEC 2015.0608 REMOVED 'INDEXED BY' FOR 'OCCURS'
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SET RBIX TO 1.
      *    SEARCH RB-TBL
      *        WHEN RB-DAYS(RBIX) NOT < T04-ELAPSED-DAYS
      *             COMPUTE REBATE-AMT ROUNDED =
      *                 REBATE-AMT
      *                    + LN-INSPREM(3) * RB-PER(RBIX - 1) / 100.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE STAT---BAD                 TO STAT.

           PERFORM VARYING RBIX FROM 1 BY 1
                     UNTIL RBIX > RBIX-MAX
                        OR STAT-GOOD
                   IF ( RB-DAYS(RBIX) NOT < T04-ELAPSED-DAYS )
                      COMPUTE REBATE-AMT ROUNDED = REBATE-AMT
                                                 + LN-INSPREM(3)
                                                 * RB-PER(RBIX - 1)
                                                 / 100
                      MOVE STAT---GOOD     TO STAT
                   END-IF
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF REBATE-AMT NOT > LN-INSPREM(3)
              COMPUTE REB-COMMON-REBATE = REBATE-AMT.
           GO TO END-PROGRAM.

       COPY "LIBGB/DATER.CPY".

      ***************************************************
       IO-ROUTINE SECTION.
      ***************************************************
      *COPY "LIBGB/CONNECT_SQL.CPY".
      *COPY "LIBGB/DECLARE_SQL.CPY".
      *COPY "LIBGB/GBGBGS_SQL.CPY".

       END-PROGRAM SECTION.
       ENDIT.
      *    PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
