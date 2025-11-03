      *    MOVE BW-VISION-KEY1   TO QBW-VISION-KEY1.
      *    MOVE BW-VISION-KEY2   TO QBW-VISION-KEY2.
      *    MOVE BW-VISION-KEY3   TO QBW-VISION-KEY3.
      *    MOVE BW-VISION-KEY4   TO QBW-VISION-KEY4.
      *================================================================*
      * END COPYBOOK: LIBLP\LPBWGS_SQL.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBGL\GLGIGS_SQL.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBGL/GLGIGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - GIFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0507 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD GI-REC FROM QGI-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-GI-FIELDS.
           MOVE QGI-BRANCH TO GI-BRANCH.
           MOVE QGI-CLASS TO GI-CLASS.

           IF ( QGI-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO GI-LTOUCH-DATE
           ELSE
              MOVE QGI-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GI-LTOUCH-DATE.

           MOVE QGI-CASH TO GI-CASH.
           MOVE QGI-BANK TO GI-BANK.
           MOVE QGI-NSF TO GI-NSF.
           MOVE QGI-AR TO GI-AR.
           MOVE QGI-PL-1 TO GI-PL(1).
           MOVE QGI-PL-2 TO GI-PL(2).
           MOVE QGI-PL-3 TO GI-PL(3).
           MOVE QGI-PL-4 TO GI-PL(4).
           MOVE QGI-PL-5 TO GI-PL(5).
           MOVE QGI-DLINSREB TO GI-DLINSREB.
           MOVE QGI-ACR-INT-AR-AR TO GI-ACR-INT-AR-AR.
           MOVE QGI-OTHBR-REC TO GI-OTHBR-REC.
           MOVE QGI-OTHBR-PAY TO GI-OTHBR-PAY.
           MOVE QGI-UNINTCHG TO GI-UNINTCHG.
           MOVE QGI-UNADDON TO GI-UNADDON.
           MOVE QGI-UNSERCHG TO GI-UNSERCHG.
           MOVE QGI-UNMAINTFEE TO GI-UNMAINTFEE.
           MOVE QGI-UNDLDISC TO GI-UNDLDISC.
           MOVE QGI-UNOTHER TO GI-UNOTHER.
           MOVE QGI-UN7 TO GI-UN7.
           MOVE QGI-UN8 TO GI-UN8.
           MOVE QGI-UN9 TO GI-UN9.
           MOVE QGI-UN10 TO GI-UN10.
           MOVE QGI-UNCL1 TO GI-UNCL1.
           MOVE QGI-UNCL2 TO GI-UNCL2.
           MOVE QGI-UNAH TO GI-UNAH.
           MOVE QGI-UNPP TO GI-UNPP.
           MOVE QGI-UNO1 TO GI-UNO1.
           MOVE QGI-UNO2 TO GI-UNO2.
           MOVE QGI-UNO3 TO GI-UNO3.
           MOVE QGI-UNO4 TO GI-UNO4.
           MOVE QGI-UNO5 TO GI-UNO5.
           MOVE QGI-UNO6 TO GI-UNO6.
           MOVE QGI-UNO7 TO GI-UNO7.
           MOVE QGI-UNINTREPO TO GI-UNINTREPO.
           MOVE QGI-UNADDONINTREPO TO GI-UNADDONINTREPO.
           MOVE QGI-PLEXP-1 TO GI-PLEXP(1).
           MOVE QGI-PLEXP-2 TO GI-PLEXP(2).
           MOVE QGI-PLEXP-3 TO GI-PLEXP(3).
           MOVE QGI-PLEXP-4 TO GI-PLEXP(4).
           MOVE QGI-PLEXP-5 TO GI-PLEXP(5).
           MOVE QGI-DLPARVEX TO GI-DLPARVEX.
           MOVE QGI-PLOFFSET-1 TO GI-PLOFFSET(1).
           MOVE QGI-PLOFFSET-2 TO GI-PLOFFSET(2).
           MOVE QGI-PLOFFSET-3 TO GI-PLOFFSET(3).
           MOVE QGI-PLOFFSET-4 TO GI-PLOFFSET(4).
           MOVE QGI-PLOFFSET-5 TO GI-PLOFFSET(5).
           MOVE QGI-SERVOFF TO GI-SERVOFF.
           MOVE QGI-PAYCL1 TO GI-PAYCL1.
           MOVE QGI-PAYCL2 TO GI-PAYCL2.
           MOVE QGI-PAYAH TO GI-PAYAH.
           MOVE QGI-PAYPP TO GI-PAYPP.
           MOVE QGI-PAYO1 TO GI-PAYO1.
           MOVE QGI-PAYO2 TO GI-PAYO2.
           MOVE QGI-PAYO3 TO GI-PAYO3.
           MOVE QGI-PAYO4 TO GI-PAYO4.
           MOVE QGI-PAYO5 TO GI-PAYO5.
           MOVE QGI-PAYO6 TO GI-PAYO6.
           MOVE QGI-PAYO7 TO GI-PAYO7.
           MOVE QGI-DLPAYABLE TO GI-DLPAYABLE.
           MOVE QGI-DLRVPRIN-1 TO GI-DLRVPRIN(1).
           MOVE QGI-DLRVPRIN-2 TO GI-DLRVPRIN(2).
           MOVE QGI-DLRVPRIN-3 TO GI-DLRVPRIN(3).
           MOVE QGI-DLRVCHGS-1 TO GI-DLRVCHGS(1).
           MOVE QGI-DLRVCHGS-2 TO GI-DLRVCHGS(2).
           MOVE QGI-DLRVCHGS-3 TO GI-DLRVCHGS(3).
           MOVE QGI-DLPARV TO GI-DLPARV.
           MOVE QGI-PAYALLOT TO GI-PAYALLOT.
           MOVE QGI-DIRECT-PAY TO GI-DIRECT-PAY.
           MOVE QGI-ERINTCHG TO GI-ERINTCHG.
           MOVE QGI-ERADDON TO GI-ERADDON.
           MOVE QGI-ERSERCHG TO GI-ERSERCHG.
           MOVE QGI-ERMAINTFEE TO GI-ERMAINTFEE.
           MOVE QGI-ERDLDISC TO GI-ERDLDISC.
           MOVE QGI-EROTHER TO GI-EROTHER.
           MOVE QGI-EARN7 TO GI-EARN7.
           MOVE QGI-EARN8 TO GI-EARN8.
           MOVE QGI-EARN9 TO GI-EARN9.
           MOVE QGI-EARN10 TO GI-EARN10.
           MOVE QGI-LATECHG TO GI-LATECHG.
           MOVE QGI-INTEREST TO GI-INTEREST.
           MOVE QGI-DEFERMENTS TO GI-DEFERMENTS.
           MOVE QGI-SAPRINCIPAL TO GI-SAPRINCIPAL.
           MOVE QGI-PLINT-1 TO GI-PLINT(1).
           MOVE QGI-PLINT-2 TO GI-PLINT(2).
           MOVE QGI-PLINT-3 TO GI-PLINT(3).
           MOVE QGI-PLINT-4 TO GI-PLINT(4).
           MOVE QGI-PLINT-5 TO GI-PLINT(5).
           MOVE QGI-PLREC-1 TO GI-PLREC(1).
           MOVE QGI-PLREC-2 TO GI-PLREC(2).
           MOVE QGI-PLREC-3 TO GI-PLREC(3).
           MOVE QGI-PLREC-4 TO GI-PLREC(4).
           MOVE QGI-PLREC-5 TO GI-PLREC(5).
           MOVE QGI-CL1 TO GI-CL1.
           MOVE QGI-CL2 TO GI-CL2.
           MOVE QGI-AH TO GI-AH.
           MOVE QGI-PP TO GI-PP.
           MOVE QGI-O1 TO GI-O1.
           MOVE QGI-O2 TO GI-O2.
           MOVE QGI-O3 TO GI-O3.
           MOVE QGI-O4 TO GI-O4.
           MOVE QGI-O5 TO GI-O5.
           MOVE QGI-O6 TO GI-O6.
           MOVE QGI-O7 TO GI-O7.
           MOVE QGI-PLNEW-AR-1 TO GI-PLNEW-AR(1).
           MOVE QGI-PLNEW-AR-2 TO GI-PLNEW-AR(2).
           MOVE QGI-PLNEW-AR-3 TO GI-PLNEW-AR(3).
           MOVE QGI-PLNEW-AR-4 TO GI-PLNEW-AR(4).
           MOVE QGI-PLNEW-AR-5 TO GI-PLNEW-AR(5).
           MOVE QGI-REPO-AR TO GI-REPO-AR.
           MOVE QGI-REPO-INT TO GI-REPO-INT.
           MOVE QGI-ACR-INT-AR TO GI-ACR-INT-AR.
           MOVE QGI-CONV-OFFSET TO GI-CONV-OFFSET.
           MOVE QGI-PARTIAL-PLEXP TO GI-PARTIAL-PLEXP.
           MOVE QGI-PAYLBOX TO GI-PAYLBOX.
           MOVE QGI-PENALTY-INT TO GI-PENALTY-INT.
           MOVE QGI-PAYTPAY TO GI-PAYTPAY.
           MOVE QGI-PAYTCOLL TO GI-PAYTCOLL.
           MOVE QGI-PAYMGRAM TO GI-PAYMGRAM.
           MOVE QGI-REPO-ADDON-EXP TO GI-REPO-ADDON-EXP.
           MOVE QGI-ACH-BANK TO GI-ACH-BANK.
           MOVE QGI-LN-APPROVAL-LIMIT TO GI-LN-APPROVAL-LIMIT.
 
      *-----------------------------------------------------------------
      *    LOAD QGI-REC FROM GI-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
