      * COPYMEMBER: LIBLP/LP01CD_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - CODE DESCRIPTION MASTER FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0430 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  CD1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  CD1-CURSOR-STAT-GOOD              VALUE "00".
           88  CD1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{NOT GREATER CURSOR FOR START-NOT-GREATER & READ-PREVIOUS}
       01  CD1-CURSOR-NG-STAT           PIC X(2) VALUE SPACES.
           88  CD1-CURSOR-NG-STAT-GOOD           VALUE "00".
           88  CD1-CURSOR-NG-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  CD1-RETURN-CODE               PIC S9(4)   COMP-5.

       01  QCD1-WHERE-BEG-KEY.
           03  QCD1-WBEG-TYPE            PIC X(2).
           03  QCD1-WBEG-CODE            PIC X(8).
 
       01  QCD1-WHERE-END-KEY.
           03  QCD1-WEND-TYPE            PIC X(2).
           03  QCD1-WEND-CODE            PIC X(8).
 
       01  QCD-REC.
           03  QCD-TYPE                   PIC X(2).
           03  QCD-CODE                   PIC X(8).
           03  QCD-LTOUCH-DATE            PIC X(10).
           03  QCD-DESC                   PIC X(35).
           03  QCD-AT-ACTIONCD            PIC X(04).
           03  QCD-AT-MEMO-REQUIRED       PIC X(01).
           03  QCD-AT-SUPERVISOR-ONLY     PIC X(01).
           03  QCD-AT-RESULT-REQUIRED     PIC X(01).
           03  QCD-BR-FILE-TYPE           PIC XX.
           03  QCD-BR-FILE-NAME           PIC X(6).
           03  QCD-BR-PAYOFF-FG           PIC X.
           03  QCD-BR-POST-SS-TRCD        PIC X.
           03  QCD-BR-GL-BRANCH           PIC 9(4).
           03  QCD-BR-GL-ACCT             PIC 9(7).
           03  QCD-BR-ALLOT-OPTION        PIC X.
           03  QCD-BR-LBOX-OPTION         PIC X.
           03  QCD-BR-LC-FG               PIC X.
           03  QCD-BR-OT-GL-BRANCH        PIC 9(4).
           03  QCD-BR-OT-GL-ACCT          PIC 9(7).
           03  QCD-BR-MULTIPLE-REFCDS     PIC X.
           03  QCD-BR-COLLECT-LC          PIC X.
           03  QCD-CO-GROUP               PIC X(01).
           03  QCD-CO-SECURED             PIC X(01).
           03  QCD-CO-TRW                 PIC X(02).
           03  QCD-CT-INSTAXFRMLA-1       PIC X.
           03  QCD-CT-INSTAXAMT-1         PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-2       PIC X.
           03  QCD-CT-INSTAXAMT-2         PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-3       PIC X.
           03  QCD-CT-INSTAXAMT-3         PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-4       PIC X.
           03  QCD-CT-INSTAXAMT-4         PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-5       PIC X.
           03  QCD-CT-INSTAXAMT-5         PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-6       PIC X.
           03  QCD-CT-INSTAXAMT-6         PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-7       PIC X.
           03  QCD-CT-INSTAXAMT-7         PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-8       PIC X.
           03  QCD-CT-INSTAXAMT-8         PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-9       PIC X.
           03  QCD-CT-INSTAXAMT-9         PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-10      PIC X.
           03  QCD-CT-INSTAXAMT-10        PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-11      PIC X.
           03  QCD-CT-INSTAXAMT-11        PIC 99V99.
           03  QCD-CT-INSTAXFRMLA-12      PIC X.
           03  QCD-CT-INSTAXAMT-12        PIC 99V99.
           03  QCD-CT-BOTH-TAX-FG         PIC X.
           03  QCD-CY-INSTAXFRMLA-1       PIC X.
           03  QCD-CY-INSTAXAMT-1         PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-2       PIC X.
           03  QCD-CY-INSTAXAMT-2         PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-3       PIC X.
           03  QCD-CY-INSTAXAMT-3         PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-4       PIC X.
           03  QCD-CY-INSTAXAMT-4         PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-5       PIC X.
           03  QCD-CY-INSTAXAMT-5         PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-6       PIC X.
           03  QCD-CY-INSTAXAMT-6         PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-7       PIC X.
           03  QCD-CY-INSTAXAMT-7         PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-8       PIC X.
           03  QCD-CY-INSTAXAMT-8         PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-9       PIC X.
           03  QCD-CY-INSTAXAMT-9         PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-10      PIC X.
           03  QCD-CY-INSTAXAMT-10        PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-11      PIC X.
           03  QCD-CY-INSTAXAMT-11        PIC 99V99.
           03  QCD-CY-INSTAXFRMLA-12      PIC X.
           03  QCD-CY-INSTAXAMT-12        PIC 99V99.
           03  QCD-CY-BOTH-TAX-FG         PIC X.
           03  QCD-EA-CQ-FG               PIC X(1).
           03  QCD-EA-GQ-FG               PIC X(1).
           03  QCD-ELGRACE                PIC 9(2).
           03  QCD-ELGRACECD              PIC X(1).
           03  QCD-ELTYPE                 PIC X(2).
           03  QCD-ELRATE                 PIC 9(4)V9(4).
           03  QCD-ELFRMLA                PIC X(2).
           03  QCD-ELMINCHG               PIC 9(2)V99.
           03  QCD-ELMAXAMT               PIC 9(4)V99.
           03  QCD-ELDELFAC               PIC 9(3).
           03  QCD-ELYRTYPE               PIC 9(4).
           03  QCD-ELUNITPER-CD           PIC X(1).
           03  QCD-PO-TITLE-HOLD-DAYS     PIC 9(02).
           03  QCD-RE-ACTIONCD            PIC X(04).
           03  QCD-RE-MEMO-REQUIRED       PIC X(01).
           03  QCD-RE-SUPERVISOR-ONLY     PIC X(01).
           03  QCD-RE-ACTIVITY-REQUIRED   PIC X(01).
           03  QCD-RE-RESCHEDULE-REQ      PIC X(01).
           03  QCD-RE-RESCHEDULE-TIME     PIC 9(03).
           03  QCD-RE-WORKED-ACCOUNT      PIC X(01).
           03  QCD-FC-FEEAMT              PIC S9(7)V99.
           03  QCD-TC-REFNO-1             PIC X(05).
           03  QCD-TC-REFNO-2             PIC X(05).
           03  QCD-TC-REFNO-3             PIC X(05).
           03  QCD-TC-REFNO-4             PIC X(05).
           03  QCD-TC-REFNO-5             PIC X(05).
           03  QCD-TC-REFNO-6             PIC X(05).
           03  QCD-TC-REFNO-7             PIC X(05).
           03  QCD-TC-REFNO-8             PIC X(05).
           03  QCD-TC-REFNO-9             PIC X(05).
           03  QCD-TC-REFNO-10            PIC X(05).
           03  QCD-PS-TDR-FLAG            PIC X(01).

       EXEC SQL END DECLARE SECTION END-EXEC.
