/*
 * Copyright (c) 2020 Broadcom.
 * The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
 *
 * This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0
 * which is available at https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *    Broadcom, Inc. - initial API and implementation
 *
 */

package com.broadcom.lsp.cobol.usecases;

import com.broadcom.lsp.cobol.usecases.engine.UseCaseEngine;
import org.eclipse.lsp4j.Diagnostic;
import org.eclipse.lsp4j.DiagnosticSeverity;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

import static com.broadcom.lsp.cobol.service.delegates.validations.SourceInfoLevels.ERROR;

/**
 * This test checks the semantic error that appears if the variable specified after THRU doesn't
 * exist
 */
class TestRenamesThruNotExists {

  private static final String TEXT =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. TESTRENAME.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
          + "       01 {$*VARNAME1}.\n"
          + "          02 {$*VARNAME2} PIC 9.\n"
          + "       66 {$*VARNAME3|1} RENAMES {$VARNAME2} THRU NOT-EXISTS.\n"
          + "       PROCEDURE DIVISION.\n"
          + "           MOVE 0 TO {$VARNAME2} OF {$VARNAME3}.\n";

  @Test
  void test() {
    UseCaseEngine.runTest(
        TEXT,
        List.of(),
        Map.of(
            "1",
            new Diagnostic(
                null,
                "The data entry with the name NOT-EXISTS not found",
                DiagnosticSeverity.Error,
                ERROR.getText())));
  }
}
