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
 *   Broadcom, Inc. - initial API and implementation
 */
import * as fs from "fs-extra";
import * as path from "path";
import * as vscode from "vscode";
import {C4Z_FOLDER, GITIGNORE_FILE} from "../../constants";
import {createFileWithGivenPath, SettingsService} from "../../services/Settings";
import {SettingsUtils} from "../../services/util/SettingsUtils";

const fsPath = "tmp-ws";
const scheme = "file";
let wsPath: string;
let c4zPath: string;
let filePath: string;

beforeAll(() => {
    (vscode.workspace.workspaceFolders as any) = [{uri: {fsPath}} as any];
    wsPath = path.join(vscode.workspace.workspaceFolders[0].uri.fsPath);
    c4zPath = path.join(wsPath, C4Z_FOLDER);
    filePath = path.join(c4zPath, GITIGNORE_FILE);
});

afterAll(() => {
    if (fs.existsSync(wsPath)) {
        fs.remove(wsPath);
    }
});

describe(".gitignore file in .c4z folder tests", () => {

    it("Create .gitignore file if not exists", () => {
        createFileWithGivenPath(C4Z_FOLDER, GITIGNORE_FILE, "/**");

        expect(fs.existsSync(wsPath)).toEqual(true);
        expect(fs.existsSync(c4zPath)).toEqual(true);
        expect(fs.existsSync(filePath)).toEqual(true);
    });

    it("Modify .gitignore file if exists", () => {
        const pattern = "srs/*\n.sds/*";
        createFileWithGivenPath(C4Z_FOLDER, GITIGNORE_FILE, pattern);
        const found = fs.readFileSync(filePath).toString().split("\n")
            .filter(e => e.trim().length > 0)
            .map(e => e.trim()).indexOf(pattern);

        expect(found).toBeGreaterThanOrEqual(-1);
    });

    it("workspace not exist", () => {
        (vscode.workspace.workspaceFolders as any) = [];
        const createFile = jest.fn();
        createFileWithGivenPath(C4Z_FOLDER, GITIGNORE_FILE, "/**");

        expect(createFile).toHaveBeenCalledTimes(0);
        expect(vscode.workspace.workspaceFolders[0]).toBe(undefined);
    });
});

describe("Validate URI generation for a given workspace folder", () => {
    test("With a valid workspace folder, the method return a valid URI representation", () => {
        const path = "/ws-vscode";
        (vscode.workspace.workspaceFolders as any) = [{uri: {path, scheme}} as any];
        expect(SettingsUtils.getWorkspacesURI()[0]).toBe("file:///ws-vscode");
    });
});

describe("SettingsService evaluate variables", () => {
    test("Evaluate fileBasenameNoExtension", () => {
        vscode.workspace.getConfiguration = jest.fn().mockReturnValue({
            get: jest.fn().mockReturnValue(["copybook/${fileBasenameNoExtension}"]),
        });
        const paths = SettingsService.getCopybookLocalPath("program", "COBOL");
        expect(paths[0]).toEqual("copybook/program");
    });

    test("Evaluate fileBasenameNoExtension", () => {
        vscode.workspace.getConfiguration = jest.fn().mockReturnValue({
            get: jest.fn().mockReturnValue(["copybook/${fileBasenameNoExtension}"]),
        });
        const paths = SettingsService.getCopybookLocalPath("program.cbl", "COBOL");
        expect(paths[0]).toEqual("copybook/program");
    });

    test("Evaluate fileBasenameNoExtension with extension and dots", () => {
        vscode.workspace.getConfiguration = jest.fn().mockReturnValue({
            get: jest.fn().mockReturnValue(["copybook/${fileBasenameNoExtension}"]),
        });
        const paths = SettingsService.getCopybookLocalPath("program.file.cbl", "COBOL");
        expect(paths[0]).toEqual("copybook/program.file");
    });

    test("Get local settings for a dialect", () => {
        const tracking = jest.fn().mockReturnValue(["copybook"]);
        vscode.workspace.getConfiguration = jest.fn().mockReturnValue({
            get: tracking,
        });
        SettingsService.getCopybookLocalPath("PROGRAM", "COBOL");
        expect(tracking).toBeCalledWith("paths-local");
    });

    test("Get local settings for dialect", () => {
        const tracking = jest.fn().mockReturnValue(["copybook"]);
        vscode.workspace.getConfiguration = jest.fn().mockReturnValue({
            get: tracking,
        });
        SettingsService.getCopybookLocalPath("PROGRAM", "MAID");
        expect(tracking).toBeCalledWith("maid.paths-local");
    });

    test("Get native build enable settings", () => {
        const tracking = jest.fn();
        vscode.workspace.getConfiguration = jest.fn().mockReturnValue({
            get: tracking,
        });
        SettingsService.serverType();
        expect(tracking).toBeCalledWith("cobol-lsp.serverType");
    });

});

describe("SettingsService returns correct tab settings", () => {
    test("Returns default tab settigs for boolean value", () => {
        vscode.workspace.getConfiguration = jest.fn().mockReturnValue({
            get: jest.fn().mockReturnValue(true),
        });

        const tabSettings = SettingsService.getTabSettings();
        expect(tabSettings.defaultRule.maxPosition).toBe(72);
    });

    test("Max position is the last threashold position for array", () => {
        vscode.workspace.getConfiguration = jest.fn().mockReturnValue({
            get: jest.fn().mockReturnValue([1, 3, 5, 7, 25]),
        });

        const tabSettings = SettingsService.getTabSettings();
        expect(tabSettings.defaultRule.maxPosition).toBe(25);
    });

    test("Different rules for different divisions with default rule", () => {
        vscode.workspace.getConfiguration = jest.fn().mockReturnValue({
            get: jest.fn().mockReturnValue({
                default: [1, 2 , 3, 40],
                anchors: {
                    "DATA +DIVISON": [1, 7, 8 , 15, 40, 52],
                    "PROCEDURE +DIVISON": [1, 7, 8, 15, 40, 45, 50],
                },
            }),
        });

        const tabSettings = SettingsService.getTabSettings();
        expect(tabSettings.defaultRule.maxPosition).toBe(40);
        expect(tabSettings.defaultRule.regex).toBeUndefined();
        expect(tabSettings.defaultRule.stops[3]).toBe(40);
        expect(tabSettings.rules.length).toBe(2);
    });

});
