      * COPYMEMBER: LIBGL/GL01GI_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - G/L GENERAL INTERFACE FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0430 ***NEW***
      * JKC 2023-1201 ADDED GI1-ROW-COUNT
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  GI1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  GI1-CURSOR-STAT-GOOD              VALUE "00".
           88  GI1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  GI1-RETURN-CODE                PIC S9(4)   COMP-5.
       01  GI1-ROW-COUNT                  PIC 9(5).

       01  QGI1-WHERE-BEG-KEY.
           03  QGI1-WBEG-BRANCH           PIC 9(04).
           03  QGI1-WBEG-CLASS            PIC 9(03).
 
       01  QGI1-WHERE-END-KEY.
           03  QGI1-WEND-BRANCH           PIC 9(04).
           03  QGI1-WEND-CLASS            PIC 9(03).
 
       01  QGI-REC.
           03  QGI-BRANCH          PIC 9(4).
           03  QGI-CLASS           PIC 999.
           03  QGI-LTOUCH-DATE     PIC X(10).
           03  QGI-CASH            PIC 9(11).
           03  QGI-BANK            PIC 9(11).
           03  QGI-NSF             PIC 9(11).
           03  QGI-AR              PIC 9(11).
           03  QGI-PL-1            PIC 9(11).
           03  QGI-PL-2            PIC 9(11).
           03  QGI-PL-3            PIC 9(11).
           03  QGI-PL-4            PIC 9(11).
           03  QGI-PL-5            PIC 9(11).
           03  QGI-DLINSREB        PIC 9(11).
           03  QGI-ACR-INT-AR-AR   PIC 9(11).
           03  QGI-OTHBR-REC       PIC 9(11).
           03  QGI-OTHBR-PAY       PIC 9(11).
           03  QGI-UNINTCHG        PIC 9(11).
           03  QGI-UNADDON         PIC 9(11).
           03  QGI-UNSERCHG        PIC 9(11).
           03  QGI-UNMAINTFEE      PIC 9(11).
           03  QGI-UNDLDISC        PIC 9(11).
           03  QGI-UNOTHER         PIC 9(11).
           03  QGI-UN7             PIC 9(11).
           03  QGI-UN8             PIC 9(11).
           03  QGI-UN9             PIC 9(11).
           03  QGI-UN10            PIC 9(11).
           03  QGI-UNCL1           PIC 9(11).
           03  QGI-UNCL2           PIC 9(11).
           03  QGI-UNAH            PIC 9(11).
           03  QGI-UNPP            PIC 9(11).
           03  QGI-UNO1            PIC 9(11).
           03  QGI-UNO2            PIC 9(11).
           03  QGI-UNO3            PIC 9(11).
           03  QGI-UNO4            PIC 9(11).
           03  QGI-UNO5            PIC 9(11).
           03  QGI-UNO6            PIC 9(11).
           03  QGI-UNO7            PIC 9(11).
           03  QGI-UNINTREPO       PIC 9(11).
           03  QGI-UNADDONINTREPO  PIC 9(11).
           03  QGI-PLEXP-1         PIC 9(11).
           03  QGI-PLEXP-2         PIC 9(11).
           03  QGI-PLEXP-3         PIC 9(11).
           03  QGI-PLEXP-4         PIC 9(11).
           03  QGI-PLEXP-5         PIC 9(11).
           03  QGI-DLPARVEX        PIC 9(11).
           03  QGI-PLOFFSET-1      PIC 9(11).
           03  QGI-PLOFFSET-2      PIC 9(11).
           03  QGI-PLOFFSET-3      PIC 9(11).
           03  QGI-PLOFFSET-4      PIC 9(11).
           03  QGI-PLOFFSET-5      PIC 9(11).
           03  QGI-SERVOFF         PIC 9(11).
           03  QGI-PAYCL1          PIC 9(11).
           03  QGI-PAYCL2          PIC 9(11).
           03  QGI-PAYAH           PIC 9(11).
           03  QGI-PAYPP           PIC 9(11).
           03  QGI-PAYO1           PIC 9(11).
           03  QGI-PAYO2           PIC 9(11).
           03  QGI-PAYO3           PIC 9(11).
           03  QGI-PAYO4           PIC 9(11).
           03  QGI-PAYO5           PIC 9(11).
           03  QGI-PAYO6           PIC 9(11).
           03  QGI-PAYO7           PIC 9(11).
           03  QGI-DLPAYABLE       PIC 9(11).
           03  QGI-DLRVPRIN-1      PIC 9(11).
           03  QGI-DLRVPRIN-2      PIC 9(11).
           03  QGI-DLRVPRIN-3      PIC 9(11).
           03  QGI-DLRVCHGS-1      PIC 9(11).
           03  QGI-DLRVCHGS-2      PIC 9(11).
           03  QGI-DLRVCHGS-3      PIC 9(11).
           03  QGI-DLPARV          PIC 9(11).
           03  QGI-PAYALLOT        PIC 9(11).
           03  QGI-DIRECT-PAY      PIC 9(11).
           03  QGI-ERINTCHG        PIC 9(11).
           03  QGI-ERADDON         PIC 9(11).
           03  QGI-ERSERCHG        PIC 9(11).
           03  QGI-ERMAINTFEE      PIC 9(11).
           03  QGI-ERDLDISC        PIC 9(11).
           03  QGI-EROTHER         PIC 9(11).
           03  QGI-EARN7           PIC 9(11).
           03  QGI-EARN8           PIC 9(11).
           03  QGI-EARN9           PIC 9(11).
           03  QGI-EARN10          PIC 9(11).
           03  QGI-LATECHG         PIC 9(11).
           03  QGI-INTEREST        PIC 9(11).
           03  QGI-DEFERMENTS      PIC 9(11).
           03  QGI-SAPRINCIPAL     PIC 9(11).
           03  QGI-PLINT-1         PIC 9(11).
           03  QGI-PLINT-2         PIC 9(11).
           03  QGI-PLINT-3         PIC 9(11).
           03  QGI-PLINT-4         PIC 9(11).
           03  QGI-PLINT-5         PIC 9(11).
           03  QGI-PLREC-1         PIC 9(11).
           03  QGI-PLREC-2         PIC 9(11).
           03  QGI-PLREC-3         PIC 9(11).
           03  QGI-PLREC-4         PIC 9(11).
           03  QGI-PLREC-5         PIC 9(11).
           03  QGI-CL1             PIC 9(11).
           03  QGI-CL2             PIC 9(11).
           03  QGI-AH              PIC 9(11).
           03  QGI-PP              PIC 9(11).
           03  QGI-O1              PIC 9(11).
           03  QGI-O2              PIC 9(11).
           03  QGI-O3              PIC 9(11).
           03  QGI-O4              PIC 9(11).
           03  QGI-O5              PIC 9(11).
           03  QGI-O6              PIC 9(11).
           03  QGI-O7              PIC 9(11).
           03  QGI-PLNEW-AR-1      PIC 9(11).
           03  QGI-PLNEW-AR-2      PIC 9(11).
           03  QGI-PLNEW-AR-3      PIC 9(11).
           03  QGI-PLNEW-AR-4      PIC 9(11).
           03  QGI-PLNEW-AR-5      PIC 9(11).
           03  QGI-REPO-AR         PIC 9(11).
           03  QGI-REPO-INT        PIC 9(11).
           03  QGI-ACR-INT-AR      PIC 9(11).
           03  QGI-CONV-OFFSET     PIC 9(11).
           03  QGI-PARTIAL-PLEXP   PIC 9(11).
           03  QGI-PAYLBOX             PIC 9(11).
           03  QGI-PENALTY-INT         PIC 9(11).
           03  QGI-PAYTPAY             PIC 9(11).
           03  QGI-PAYTCOLL            PIC 9(11).
           03  QGI-PAYMGRAM            PIC 9(11).
           03  QGI-REPO-ADDON-EXP      PIC 9(11).
           03  QGI-ACH-BANK            PIC 9(11).
           03  QGI-LN-APPROVAL-LIMIT   PIC 9(7).


       EXEC SQL END DECLARE SECTION END-EXEC.
