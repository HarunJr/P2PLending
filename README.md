# P2PLending
Simple Plutus app demonstrating how to create a loan request, cancel the request and lend s Native token with Ada as collateral

## Borrowing
User can Request for a loan by sending Ada as collateral with Datum to the Request.hs Contract. An NFT is minted in the process (as a receipt).
Only the user with the NFT can retrieve the collateral from Request.hs.

## Canceling
User can cancel transaction by burning the NFT and Request.hs sends back the collateral.

## Lending
User can lend USDH  to another user with Ada as collateral. Only when When Lender NFT is created and the collateral at Request.hs is moved to Collateral.hs
is USDH sent to the borrower.

