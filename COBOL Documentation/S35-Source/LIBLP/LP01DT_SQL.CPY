      * COPYMEMBER: LIBLP/LP01DT_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - DEALER TRANSACTION FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  DT1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  DT1-CURSOR-STAT-GOOD              VALUE "00".
           88  DT1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  DT2-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  DT2-CURSOR-STAT-GOOD              VALUE "00".
           88  DT2-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  DT3-CURSOR-STAT           PIC X(2)    VALUE SPACES.
           88  DT3-CURSOR-STAT-GOOD              VALUE "00".
           88  DT3-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  DT4-CURSOR-STAT           PIC X(2)    VALUE SPACES.
           88  DT4-CURSOR-STAT-GOOD              VALUE "00".
           88  DT4-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  DT1-RETURN-CODE                 PIC S9(4)   COMP-5.
       01  DT2-RETURN-CODE                 PIC S9(4)   COMP-5.
       01  DT3-RETURN-CODE                 PIC S9(4)   COMP-5.
       01  DT4-RETURN-CODE                 PIC S9(4)   COMP-5.

       01  QDT1-WHERE-BEG-KEY.
           03  QDT1-WBEG-BRNO              PIC 9(04).
           03  QDT1-WBEG-NUM               PIC 9(06).
           03  QDT1-WBEG-CLASS             PIC 9(03).
           03  QDT1-WBEG-ACCTNO            PIC 9(08).
           03  QDT1-WBEG-DATE              PIC X(10).
           03  QDT1-WBEG-SEQ               PIC 9(03).
 
       01  QDT1-WHERE-END-KEY.
           03  QDT1-WEND-BRNO              PIC 9(04).
           03  QDT1-WEND-NUM               PIC 9(06).
           03  QDT1-WEND-CLASS             PIC 9(03).
           03  QDT1-WEND-ACCTNO            PIC 9(08).
           03  QDT1-WEND-DATE              PIC X(10).
           03  QDT1-WEND-SEQ               PIC 9(03).
 
       01  QDT2-WHERE-BEG-KEY.
           03  QDT2-WBEG-BRNO              PIC 9(04).
           03  QDT2-WBEG-BATCHID           PIC 9(10).
           03  QDT2-WBEG-BATCHID-SEQ       PIC 9(05).

       01  QDT2-WHERE-END-KEY.
           03  QDT2-WEND-BRNO              PIC 9(04).
           03  QDT2-WEND-BATCHID           PIC 9(10).
           03  QDT2-WEND-BATCHID-SEQ       PIC 9(05).

       01  QDT3-WHERE-BEG-KEY.
           03  QDT3-WBEG-STATUS            PIC X.
           03  QDT3-WBEG-BRNO              PIC 9(04).
           03  QDT3-WBEG-NUM               PIC 9(06).
           03  QDT3-WBEG-CLASS             PIC 9(03).
           03  QDT3-WBEG-ACCTNO            PIC 9(08).
           03  QDT3-WBEG-DATE              PIC X(10).
           03  QDT3-WBEG-SEQ               PIC 9(03).
 
       01  QDT3-WHERE-END-KEY.
           03  QDT3-WEND-STATUS            PIC X.
           03  QDT3-WEND-BRNO              PIC 9(04).
           03  QDT3-WEND-NUM               PIC 9(06).
           03  QDT3-WEND-CLASS             PIC 9(03).
           03  QDT3-WEND-ACCTNO            PIC 9(08).
           03  QDT3-WEND-DATE              PIC X(10).
           03  QDT3-WEND-SEQ               PIC 9(03).
 
       01  QDT4-WHERE-BEG-KEY.
           03  QDT4-WBEG-BRNO              PIC 9(04).
           03  QDT4-WBEG-CKNO              PIC 9(06).

       01  QDT4-WHERE-END-KEY.
           03  QDT4-WEND-BRNO              PIC 9(04).
           03  QDT4-WEND-CKNO              PIC 9(06).

       01  QDT-REC.
           03  QDT-STATUS                PIC X.
           03  QDT-BRNO                  PIC 9(4).
           03  QDT-NUM                   PIC 9(6).
           03  QDT-CLASS                 PIC 9(3).
           03  QDT-ACCTNO                PIC 9(8).
           03  QDT-DATE                  PIC X(10).
           03  QDT-SEQ                   PIC 9(3).
           03  QDT-LTOUCH-DATE           PIC X(10).
           03  QDT-TYPE                  PIC X.
           03  QDT-LPSEQNO               PIC 9(5).
           03  QDT-TRCD                  PIC XX.
           03  QDT-DLRVPRIN-1            PIC S9(7)V99.
           03  QDT-DLRVPRIN-2            PIC S9(7)V99.
           03  QDT-DLRVPRIN-3            PIC S9(7)V99.
           03  QDT-DLRVCHGS-1            PIC S9(7)V99.
           03  QDT-DLRVCHGS-2            PIC S9(7)V99.
           03  QDT-DLRVCHGS-3            PIC S9(7)V99.
           03  QDT-DLPARVHELD            PIC S9(7)V99.
           03  QDT-DLPARVPAID            PIC S9(7)V99.
           03  QDT-PROCEEDS              PIC S9(7)V99.
           03  QDT-RELEASED              PIC S9(7)V99.
           03  QDT-PAYMENTS              PIC S9(7)V99.
           03  QDT-CKNO                  PIC 9(6).
           03  QDT-CKDATE                PIC X(10).
           03  QDT-BATCHID               PIC 9(10).
           03  QDT-BATCHID-SEQ           PIC 9(5).
           03  QDT-PAID                  PIC X.
           03  QDT-REVERSE               PIC X.
           03  QDT-REVERSE-TRCD          PIC XX.
           03  QDT-PURBAL                PIC S9(7)V99.
           03  QDT-APCUR                 PIC S9(7)V99.
           03  QDT-DLRVNO                PIC 9.
           03  QDT-SELECTED              PIC X.
           03  QDT-P4P5-TRCD             PIC XX.
           03  QDT-DLRVPRIN-PAY          PIC S9(7)V99.
           03  QDT-DLRVCHGS-PAY          PIC S9(7)V99.
           03  QDT-DLPARVHELD-PAY        PIC S9(7)V99.
           03  QDT-CHECK-VOID            PIC X.
           03  QDT-UNPAID                PIC X.
           03  QDT-MEMO                  PIC X.
           03  QDT-EVENT-CD              PIC X(2).
           03  QDT-PART-HELD-STATUS      PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.
