!pip install mistralai

import os
import re
from mistralai import Mistral

api_key = "2i0tgxeGBl8J8VB4j9VU5jG79m7tHbCp"
model = "mistral-large-latest"
client = Mistral(api_key=api_key)ROGRAM-ID. BankOverflow.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 TOTAL-AMOUNT     PIC 9(5) VALUE ZEROS.
01 TRANSACTION-AMT  PIC 9(5).

PROCEDURE DIVISION.
    DISPLAY "Enter transaction amount: " WITH NO ADVANCING.
    ACCEPT TRANSACTION-AMT.

    ADD TRANSACTION-AMT TO TOTAL-AMOUNT.

    IF TOTAL-AMOUNT > 99999
        DISPLAY "ERROR: Overflow occurred in total amount calculation."
    ELSE
        DISPLAY "Updated Total Amount: " TOTAL-AMOUNT.
    END-IF.

chat_response = client.chat.complete(
    model= model,
    messages = [
        {
            "role": "user",
            "content": """Fix the issues in this COBOL code: IDENTIFICATION DIVISION.
PROGRAM-ID. BankOverflow.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 TOTAL-AMOUNT     PIC 9(5) VALUE ZEROS.
01 TRANSACTION-AMT  PIC 9(5).

PROCEDURE DIVISION.
    DISPLAY "Enter transaction amount: " WITH NO ADVANCING.
    ACCEPT TRANSACTION-AMT.

    ADD TRANSACTION-AMT TO TOTAL-AMOUNT.

    IF TOTAL-AMOUNT > 99999
        DISPLAY "ERROR: Overflow occurred in total amount calculation."
    ELSE
        DISPLAY "Updated Total Amount: " TOTAL-AMOUNT.
    END-IF.

    STOP RUN.""",
        },
    ]
)

output = chat_response.choices[0].message.content

def extract_cobol_code(output: str) -> str:
    match = re.search(r"```cobol\s+(.*?)\s+```", output, re.DOTALL | re.IGNORECASE)

    if match:
        cobol_code = match.group(1).strip()
        return cobol_code
    else:
        fallback_match = re.search(r"```cobol\s+(.*?)\s+```", output, re.DOTALL | re.IGNORECASE)
        if fallback_match:
            return fallback_match.group(1).strip()
        else:
            return "No COBOL code found in the output."

if __name__ == "__main__":
    clean_code = extract_cobol_code(output)
    print(clean_code)

