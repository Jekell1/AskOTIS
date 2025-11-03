      * COPYMEMBER: LIBUP/UPATRSCL
      ***********************************************************************
       CLEAR-ATRS-FILE SECTION.
      ***********************************************************************
           MOVE ALL X"09" TO ATRS-REC.

           MOVE 0 TO
              ATS-DATE ATS-BRNO ATS-CLASS ATS-ACCTNO 
              ATS-LAWCODE ATS-LINE ATS-LOANDATE ATS-INTDATE
              ATS-PROCEEDS ATS-LNAMT ATS-FINCHG ATS-INTCHG ATS-SERCHG
              ATS-EXTCHG ATS-MAINTFEE ATS-ORGTERM ATS-1STPYAMT
              ATS-NOREGPY ATS-REGPYAMT ATS-LASTPYAMT
              ATS-DLCOMM-PP-CMSI ATS-DLCOMM-O3-CMSI ATS-TILLNO
              ATS-DLNO ATS-DLPARTRCAMT ATS-PURBAL 
              ATS-DLPARVHELD  ATS-DLPARVPAID ATS-DLDISC
              ATS-ANTICTERM ATS-PRLNNO ATS-INSPREM-DLADJ
              ATS-PRDLDISC-UNEARN ATS-SPRCLASS ATS-O3INS-DLPROC
              ATS-POSTTIME ATS-POSTDATE.

              MOVE 0 TO ATS-SUBCLASS.
              MOVE 0 TO ATS-TAX-AMT ATS-APPROVED-BY-DATE,
                        ATS-APPROVED-BY-TIME ATS-LTOUCH-DATE.

              MOVE 0 TO ATS-DLRVPRIN(1) ATS-DLRVPRIN(2)
                        ATS-DLRVPRIN(3) ATS-DLRVCHGS(1)
                        ATS-DLRVCHGS(2) ATS-DLRVCHGS(3).

           MOVE SPACES TO ATS-PURCD ATS-SRCD ATS-COLLCD ATS-SERVACCT 
                          ATS-LOANTYPE ATS-INSURED ATS-INSTYPES
                          ATS-INSELIGIBLE ATS-INS-TAX-COUNTY-CD
                          ATS-INS-TAX-CITY-CD ATS-UNWIND-REASON-CODE
                          ATS-DLRECOURSE ATS-DL-RVCHGS-CODE(1)
                          ATS-DL-RVCHGS-CODE(2) ATS-DL-RVCHGS-CODE(3)
                          ATS-DL-PARVHELD-CODE ATS-DL-PARVPAID-CODE
                          ATS-SP-LNCOST-FRMLA ATS-DLPAID ATS-MIP-FG
                          ATS-USERID ATS-APPROVED-BY-USER 
                          ATS-APPROVED-BY-CODE.

              PERFORM CLEAR-ATRS-FILE-TABLES
                   VARYING TRSSUB FROM 1 BY 1 UNTIL TRSSUB > 20.

      *******************************************
       CLEAR-ATRS-FILE-TABLES SECTION.
      *******************************************
           IF TRSSUB < 5
              MOVE 0      TO ATS-SSNO(TRSSUB)
              MOVE SPACES TO ATS-MAKERCD(TRSSUB).
           IF TRSSUB < 11
              MOVE 0 TO ATS-EARNINGS(TRSSUB)    ATS-ANTICERN(TRSSUB)
              MOVE 0 TO ATS-INSCOMP(TRSSUB)     ATS-INSEFF-DATE(TRSSUB)
                        ATS-INSEXP-DATE(TRSSUB) ATS-INSPREM(TRSSUB)
                        ATS-INSCOMM(TRSSUB)     ATS-INSOURS(TRSSUB)
                        ATS-INSANTICERN(TRSSUB) ATS-INSEARNINGS(TRSSUB).
 
           MOVE 0      TO ATS-FEEAMT(TRSSUB).
           MOVE SPACES TO ATS-FEECODE(TRSSUB)
                          ATS-FEEOURS(TRSSUB).

