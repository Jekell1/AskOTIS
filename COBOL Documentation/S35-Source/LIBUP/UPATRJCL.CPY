      * COPYMEMBER: LIBUP/UPATRJCL

      ***********************************************************************
      *        POSTING TRANSACTION FILE CLEAR ROUTINE
      *                   CLASS TRANSFER RECORDS
      *
      ***********************************************************************
       CLEAR-ATRJ-FILE SECTION.
           MOVE ALL X"09" TO ATRJ-REC.

           MOVE 0 TO ATJ-DATE ATJ-BRNO ATJ-CLASS ATJ-ACCTNO 
                     ATJ-LAWCODE ATJ-LINE ATJ-LTOUCH-DATE ATJ-LOANDATE
                     ATJ-PROCEEDS ATJ-LNAMT ATJ-FINCHG ATJ-INTCHG
                     ATJ-SERCHG ATJ-EXTCHG ATJ-MAINTFEE ATJ-ORGTERM
                     ATJ-DLNO ATJ-DLRV ATJ-DLSPRV ATJ-DLDISC
                     ATJ-DLPARV ATJ-DLPARTRCAMT ATJ-PURBAL
                     ATJ-PRLNNO ATJ-DLPARVPAID ATJ-DLPARVHELD
                     ATJ-POSTDATE ATJ-POSTTIME ATJ-SUBCLASS.
           MOVE 0   TO ATJ-FRLC ATJ-TOLC.

           MOVE SPACES TO ATJ-REFNO ATJ-PURCD ATJ-SRCD ATJ-COLLCD
                          ATJ-SERVACCT ATJ-LOANTYPE ATJ-INSURED
                          ATJ-DLRECOURSE ATJ-USERID ATJ-PLCD 
                          ATJ-PLTYPE ATJ-REPOCD.

           PERFORM CLEAR-ATRJ-FILE-TABLES
                   VARYING TRJSUB FROM 1 BY 1 UNTIL TRJSUB > 10.

      ************************************************
       CLEAR-ATRJ-FILE-TABLES SECTION.
      ************************************************
           IF TRJSUB < 4
              MOVE 0 TO ATJ-DLRVPRIN(TRJSUB) ATJ-DLRVCHGS(TRJSUB).
           IF TRJSUB < 5
              MOVE 0 TO ATJ-SSNO(TRJSUB)
              MOVE SPACES TO ATJ-MAKERCD(TRJSUB).
 
           MOVE 0 TO ATJ-UNEARN(TRJSUB) ATJ-INSUNEARN(TRJSUB).
 
 
