      * COPYMEMBER: LIBUP/UPATRXCL

      ***********************************************************************
       CLEAR-ATRX-FILE SECTION.
      ***********************************************************************
           MOVE ALL X"09" TO ATRX-REC.

           MOVE 0 TO
              ATX-DATE ATX-BRNO ATX-CLASS ATX-ACCTNO 
              ATX-LAWCODE ATX-LINE ATX-LTOUCH-DATE ATX-LOANDATE 
              ATX-PROCEEDS
              ATX-LNAMT ATX-FINCHG ATX-INTCHG ATX-SERCHG ATX-EXTCHG
              ATX-MAINTFEE ATX-ORGTERM ATX-DLNO ATX-DLPARTRCAMT
              ATX-PURBAL
              ATX-DLPARVHELD ATX-DLPARVPAID ATX-DLDISC
              ATX-ANTICTERM ATX-PRLNNO ATX-PRBAL ATX-TFRBAL
              ATX-SPRCLASS ATX-POSTTIME ATX-POSTDATE.

              MOVE 0 TO ATX-SUBCLASS.

              MOVE SPACES TO ATX-PURCD ATX-SRCD ATX-COLLCD
                             ATX-SERVACCT ATX-LOANTYPE
                             ATX-INSURED ATX-DLRECOURSE ATX-USERID.

              PERFORM CLEAR-ATRX-FILE-TABLES
                   VARYING TRXSUB FROM 1 BY 1 UNTIL TRXSUB > 10.

      **********************************************
       CLEAR-ATRX-FILE-TABLES SECTION.
      **********************************************
              IF TRXSUB < 4
                 MOVE 0 TO ATX-DLRVPRIN(TRXSUB) ATX-DLRVCHGS(TRXSUB).
              IF TRXSUB < 5
                 MOVE 0 TO ATX-SSNO(TRXSUB)
                 MOVE SPACES TO ATX-MAKERCD(TRXSUB).

      * OCCURS 10
              MOVE 0 TO ATX-EARNINGS(TRXSUB)    ATX-ANTICERN(TRXSUB) 
              MOVE 0 TO ATX-INSANTICERN(TRXSUB) ATX-INSEARNINGS(TRXSUB).


