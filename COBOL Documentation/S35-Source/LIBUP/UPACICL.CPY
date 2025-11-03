      * COPYMEMBER: LIBUP/UPACICL

      ***********************************************************************
      *       CLEAR ASCII CUSTOMER INFOMATION FILE
      *
      ***********************************************************************
       CLEAR-ACI-FILE SECTION.
           MOVE ALL X"09" TO ACI-REC.

           MOVE 0 TO ACI-ACCTNO              ACI-SSNO ACI-BRNO
                     ACI-SSNO-2              ACI-DATE-UPDATED
                     ACI-AMT-REQUESTED       ACI-YEARS-IN-AREA
                     ACI-DATE-AT-PRESENT-ADR ACI-MONTHLY-RENT-MTG
                     ACI-FAIR-MARKET-VALUE   ACI-MTG-BALANCE
                     ACI-TOTAL-MO-INCOME     ACI-LESS-MO-PAY
                     ACI-DATE-HIRE-1         ACI-GROSS-MO-INCOME-1
                     ACI-DATE-HIRE-2         ACI-GROSS-MO-INCOME-2.

           MOVE SPACES TO ACI-MARITAL-STAT ACI-CHECKING-ACCOUNT
                          ACI-SAVINGS-ACCOUNT ACI-POSITION-CODE-1
                          ACI-POSITION-CODE-2.

