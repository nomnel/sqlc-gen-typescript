import {
  SyntaxKind,
  NodeFlags,
  TypeNode,
  factory,
  FunctionDeclaration,
} from "typescript";

import { Parameter, Column, Query } from "../gen/plugin/codegen_pb";
import { argName, colName } from "./utlis";

function funcParamsDecl(iface: string | undefined, params: Parameter[]) {
  let funcParams = [
    factory.createParameterDeclaration(
      undefined,
      undefined,
      factory.createIdentifier("conn"),
      undefined,
      factory.createUnionTypeNode([
        factory.createTypeReferenceNode(
          factory.createIdentifier("Connection"),
          undefined
        ),
        factory.createTypeReferenceNode(
          factory.createIdentifier("Tx"),
          undefined
        )
      ]),
      undefined
    ),
  ];

  if (iface && params.length > 0) {
    funcParams.push(
      factory.createParameterDeclaration(
        undefined,
        undefined,
        factory.createIdentifier("args"),
        undefined,
        factory.createTypeReferenceNode(
          factory.createIdentifier(iface),
          undefined
        ),
        undefined
      )
    );
  }

  return funcParams;
}

export class Driver {
  columnType(column?: Column): TypeNode {
    if (column === undefined || column.type === undefined) {
      return factory.createKeywordTypeNode(SyntaxKind.AnyKeyword);
    }
    let typ: TypeNode = factory.createKeywordTypeNode(SyntaxKind.StringKeyword);

    switch (column.type.name) {
      case "bit": {
        typ = factory.createTypeReferenceNode(
          factory.createIdentifier("Buffer"),
          undefined
        );
        break;
      }
      case "tinyint":
      case "bool":
      case "boolean":
      case "smallint":
      case "mediumint":
      case "int":
      case "integer": {
        typ = factory.createKeywordTypeNode(SyntaxKind.NumberKeyword);
        break;
      }
      case "bigint":
      case "serial": {
        typ = factory.createKeywordTypeNode(SyntaxKind.StringKeyword);
        break;
      }
      case "decimal":
      case "dec":
      case "numeric":
      case "fixed": {
        typ = factory.createKeywordTypeNode(SyntaxKind.StringKeyword);
        break;
      }
      case "float":
      case "double":
      case "double precision": {
        typ = factory.createKeywordTypeNode(SyntaxKind.NumberKeyword);
        break;
      }
      case "date":
      case "datetime":
      case "timestamp": {
        typ = factory.createTypeReferenceNode(
          factory.createIdentifier("Date"),
          undefined
        );
        break;
      }
      case "time": {
        typ = factory.createKeywordTypeNode(SyntaxKind.StringKeyword);
        break;
      }
      case "year": {
        typ = factory.createKeywordTypeNode(SyntaxKind.NumberKeyword);
        break;
      }
      case "char":
      case "nchar":
      case "national char":
      case "varchar": {
        typ = factory.createKeywordTypeNode(SyntaxKind.StringKeyword);
        break;
      }
      case "binary":
      case "varbinary":
      case "tinyblob":
      case "blob":
      case "mediumblob":
      case "longblob": {
        typ = factory.createTypeReferenceNode(
          factory.createIdentifier("Buffer"),
          undefined
        );
        break;
      }
      case "tinytext":
      case "text":
      case "mediumtext":
      case "longtext": {
        typ = factory.createKeywordTypeNode(SyntaxKind.StringKeyword);
        break;
      }
      case "json": {
        typ = factory.createKeywordTypeNode(SyntaxKind.AnyKeyword);
        break;
      }
    }
    if (column.notNull) {
      return typ;
    }
    return factory.createUnionTypeNode([
      typ,
      factory.createLiteralTypeNode(factory.createNull()),
    ]);
  }

  preamble(queries: Query[]) {
    const hasExecLastIdCmd = queries.some(
      (query) => query.cmd === ":execlastid"
    );
    return [
      factory.createImportDeclaration(
        undefined,
        factory.createImportClause(
          false,
          undefined,
          factory.createNamedImports([
            factory.createImportSpecifier(
              false,
              undefined,
              factory.createIdentifier("Connection")
            ),
            factory.createImportSpecifier(
              false,
              undefined,
              factory.createIdentifier("Row")
            ),
            factory.createImportSpecifier(
              false,
              undefined,
              factory.createIdentifier("Tx")
            ),
            ...(hasExecLastIdCmd
              ? [
                  factory.createImportSpecifier(
                    false,
                    undefined,
                    factory.createIdentifier("FullResult")
                  ),
                ]
              : []),
          ])
        ),
        factory.createStringLiteral("@tidbcloud/serverless"),
        undefined
      ),
    ];
  }

  execDecl(
    funcName: string,
    queryName: string,
    argIface: string | undefined,
    params: Parameter[]
  ) {
    const funcParams = funcParamsDecl(argIface, params);

    return factory.createFunctionDeclaration(
      [
        factory.createToken(SyntaxKind.ExportKeyword),
        factory.createToken(SyntaxKind.AsyncKeyword),
      ],
      undefined,
      factory.createIdentifier(funcName),
      undefined,
      funcParams,
      factory.createTypeReferenceNode(factory.createIdentifier("Promise"), [
        factory.createKeywordTypeNode(SyntaxKind.VoidKeyword),
      ]),
      factory.createBlock(
        [
          factory.createExpressionStatement(
            factory.createAwaitExpression(
              factory.createCallExpression(
                factory.createPropertyAccessExpression(
                  factory.createIdentifier("conn"),
                  factory.createIdentifier("execute")
                ),
                undefined,
                [
                  factory.createIdentifier(queryName),
                  factory.createArrayLiteralExpression(
                    params.map((param, i) =>
                      factory.createPropertyAccessExpression(
                        factory.createIdentifier("args"),
                        factory.createIdentifier(argName(i, param.column))
                      )
                    ),
                    false
                  ),
                ]
              )
            )
          ),
        ],
        true
      )
    );
  }

  oneDecl(
    funcName: string,
    queryName: string,
    argIface: string | undefined,
    returnIface: string,
    params: Parameter[],
    columns: Column[]
  ) {
    const funcParams = funcParamsDecl(argIface, params);

    return factory.createFunctionDeclaration(
      [
        factory.createToken(SyntaxKind.ExportKeyword),
        factory.createToken(SyntaxKind.AsyncKeyword),
      ],
      undefined,
      factory.createIdentifier(funcName),
      undefined,
      funcParams,
      factory.createTypeReferenceNode(factory.createIdentifier("Promise"), [
        factory.createUnionTypeNode([
          factory.createTypeReferenceNode(
            factory.createIdentifier(returnIface),
            undefined
          ),
          factory.createLiteralTypeNode(factory.createNull()),
        ]),
      ]),
      factory.createBlock(
        [
          factory.createVariableStatement(
            undefined,
            factory.createVariableDeclarationList(
              [
                factory.createVariableDeclaration(
                  factory.createIdentifier("result"),
                  undefined,
                  undefined,
                  factory.createAwaitExpression(
                    factory.createCallExpression(
                      factory.createPropertyAccessExpression(
                        factory.createIdentifier("conn"),
                        factory.createIdentifier("execute")
                      ),
                      undefined,
                      [
                        factory.createIdentifier(queryName),
                        factory.createArrayLiteralExpression(
                          params.map((param, i) =>
                            factory.createPropertyAccessExpression(
                              factory.createIdentifier("args"),
                              factory.createIdentifier(
                                argName(i, param.column)
                              )
                            )
                          ),
                          false
                        ),
                        factory.createObjectLiteralExpression(
                          [
                            factory.createPropertyAssignment(
                              factory.createIdentifier("arrayMode"),
                              factory.createTrue()
                            ),
                          ],
                          false
                        ),
                      ]
                    )
                  )
                ),
              ],
              NodeFlags.Const |
                NodeFlags.AwaitContext |
                NodeFlags.ContextFlags |
                NodeFlags.TypeExcludesFlags
            )
          ),
          factory.createIfStatement(
            factory.createBinaryExpression(
              factory.createPrefixUnaryExpression(
                SyntaxKind.ExclamationToken,
                factory.createCallExpression(
                  factory.createPropertyAccessExpression(
                    factory.createIdentifier("Array"),
                    factory.createIdentifier("isArray")
                  ),
                  undefined,
                  [factory.createIdentifier("result")]
                )
              ),
              factory.createToken(SyntaxKind.BarBarToken),
              factory.createBinaryExpression(
                factory.createPropertyAccessExpression(
                  factory.createIdentifier("result"),
                  factory.createIdentifier("length")
                ),
                factory.createToken(SyntaxKind.ExclamationEqualsEqualsToken),
                factory.createNumericLiteral("1")
              )
            ),
            factory.createBlock(
              [factory.createReturnStatement(factory.createNull())],
              true
            ),
            undefined
          ),
          factory.createVariableStatement(
            undefined,
            factory.createVariableDeclarationList(
              [
                factory.createVariableDeclaration(
                  factory.createIdentifier("row"),
                  undefined,
                  undefined,
                  factory.createAsExpression(
                    factory.createElementAccessExpression(
                      factory.createIdentifier("result"),
                      factory.createNumericLiteral("0")
                    ),
                    factory.createArrayTypeNode(
                      factory.createKeywordTypeNode(SyntaxKind.AnyKeyword)
                    )
                  )
                ),
              ],
              NodeFlags.Const |
                NodeFlags.AwaitContext |
                NodeFlags.ContextFlags |
                NodeFlags.TypeExcludesFlags
            )
          ),
          factory.createReturnStatement(
            factory.createObjectLiteralExpression(
              columns.map((col, i) =>
                factory.createPropertyAssignment(
                  factory.createIdentifier(colName(i, col)),
                  factory.createElementAccessExpression(
                    factory.createIdentifier("row"),
                    factory.createNumericLiteral(`${i}`)
                  )
                )
              ),
              true
            )
          ),
        ],
        true
      )
    );
  }

  manyDecl(
    funcName: string,
    queryName: string,
    argIface: string | undefined,
    returnIface: string,
    params: Parameter[],
    columns: Column[]
  ) {
    const funcParams = funcParamsDecl(argIface, params);

    return factory.createFunctionDeclaration(
      [
        factory.createToken(SyntaxKind.ExportKeyword),
        factory.createToken(SyntaxKind.AsyncKeyword),
      ],
      undefined,
      factory.createIdentifier(funcName),
      undefined,
      funcParams,
      factory.createTypeReferenceNode(factory.createIdentifier("Promise"), [
        factory.createArrayTypeNode(
          factory.createTypeReferenceNode(
            factory.createIdentifier(returnIface),
            undefined
          )
        ),
      ]),
      factory.createBlock(
        [
          factory.createVariableStatement(
            undefined,
            factory.createVariableDeclarationList(
              [
                factory.createVariableDeclaration(
                  factory.createIdentifier("result"),
                  undefined,
                  undefined,
                  factory.createAwaitExpression(
                    factory.createCallExpression(
                      factory.createPropertyAccessExpression(
                        factory.createIdentifier("conn"),
                        factory.createIdentifier("execute")
                      ),
                      undefined,
                      [
                        factory.createIdentifier(queryName),
                        factory.createArrayLiteralExpression(
                          params.map((param, i) =>
                            factory.createPropertyAccessExpression(
                              factory.createIdentifier("args"),
                              factory.createIdentifier(
                                argName(i, param.column)
                              )
                            )
                          ),
                          false
                        ),
                        factory.createObjectLiteralExpression(
                          [
                            factory.createPropertyAssignment(
                              factory.createIdentifier("arrayMode"),
                              factory.createTrue()
                            ),
                          ],
                          false
                        ),
                      ]
                    )
                  )
                ),
              ],
              NodeFlags.Const |
                NodeFlags.AwaitContext |
                NodeFlags.ContextFlags |
                NodeFlags.TypeExcludesFlags
            )
          ),
          factory.createIfStatement(
            factory.createPrefixUnaryExpression(
              SyntaxKind.ExclamationToken,
              factory.createCallExpression(
                factory.createPropertyAccessExpression(
                  factory.createIdentifier("Array"),
                  factory.createIdentifier("isArray")
                ),
                undefined,
                [factory.createIdentifier("result")]
              )
            ),
            factory.createBlock(
              [
                factory.createReturnStatement(
                  factory.createArrayLiteralExpression([], false)
                ),
              ],
              true
            ),
            undefined
          ),
          factory.createReturnStatement(
            factory.createCallExpression(
              factory.createPropertyAccessExpression(
                factory.createParenthesizedExpression(
                  factory.createAsExpression(
                    factory.createIdentifier("result"),
                    factory.createArrayTypeNode(
                      factory.createArrayTypeNode(
                        factory.createKeywordTypeNode(SyntaxKind.AnyKeyword)
                      )
                    )
                  )
                ),
                factory.createIdentifier("map")
              ),
              undefined,
              [
                factory.createArrowFunction(
                  undefined,
                  undefined,
                  [
                    factory.createParameterDeclaration(
                      undefined,
                      undefined,
                      factory.createIdentifier("row"),
                      undefined,
                      undefined,
                      undefined
                    ),
                  ],
                  undefined,
                  factory.createToken(SyntaxKind.EqualsGreaterThanToken),
                  factory.createBlock(
                    [
                      factory.createReturnStatement(
                        factory.createObjectLiteralExpression(
                          columns.map((col, i) =>
                            factory.createPropertyAssignment(
                              factory.createIdentifier(colName(i, col)),
                              factory.createElementAccessExpression(
                                factory.createIdentifier("row"),
                                factory.createNumericLiteral(`${i}`)
                              )
                            )
                          ),
                          true
                        )
                      ),
                    ],
                    true
                  )
                ),
              ]
            )
          ),
        ],
        true
      )
    );
  }

  execlastidDecl(
    funcName: string,
    queryName: string,
    argIface: string | undefined,
    params: Parameter[]
  ) {
    const funcParams = funcParamsDecl(argIface, params);

    return factory.createFunctionDeclaration(
      [
        factory.createToken(SyntaxKind.ExportKeyword),
        factory.createToken(SyntaxKind.AsyncKeyword),
      ],
      undefined,
      factory.createIdentifier(funcName),
      undefined,
      funcParams,
      factory.createTypeReferenceNode(factory.createIdentifier("Promise"), [
        factory.createTypeReferenceNode("number", undefined),
      ]),
      factory.createBlock(
        [
          factory.createVariableStatement(
            undefined,
            factory.createVariableDeclarationList(
              [
                factory.createVariableDeclaration(
                  factory.createIdentifier("result"),
                  undefined,
                  undefined,
                  factory.createAwaitExpression(
                    factory.createCallExpression(
                      factory.createPropertyAccessExpression(
                        factory.createIdentifier("conn"),
                        factory.createIdentifier("execute")
                      ),
                      undefined,
                      [
                        factory.createIdentifier(queryName),
                        factory.createArrayLiteralExpression(
                          params.map((param, i) =>
                            factory.createPropertyAccessExpression(
                              factory.createIdentifier("args"),
                              factory.createIdentifier(
                                argName(i, param.column)
                              )
                            )
                          ),
                          false
                        ),
                        factory.createObjectLiteralExpression(
                          [
                            factory.createPropertyAssignment(
                              factory.createIdentifier("fullResult"),
                              factory.createTrue()
                            ),
                          ],
                          false
                        ),
                      ]
                    )
                  )
                ),
              ],
              NodeFlags.Const |
                NodeFlags.AwaitContext |
                NodeFlags.ContextFlags |
                NodeFlags.TypeExcludesFlags
            )
          ),
          factory.createIfStatement(
            factory.createPrefixUnaryExpression(
              SyntaxKind.ExclamationToken,
              factory.createCallExpression(
                factory.createPropertyAccessExpression(
                  factory.createIdentifier("Array"),
                  factory.createIdentifier("isArray")
                ),
                undefined,
                [factory.createIdentifier("result")]
              )
            ),
            factory.createBlock(
              [
                factory.createReturnStatement(
                  factory.createBinaryExpression(
                    factory.createCallExpression(
                      factory.createIdentifier("Number"),
                      undefined,
                      [
                        factory.createPropertyAccessExpression(
                          factory.createIdentifier("result"),
                          factory.createIdentifier("lastInsertId")
                        ),
                      ]
                    ),
                    factory.createToken(SyntaxKind.BarBarToken),
                    factory.createNumericLiteral("0")
                  )
                ),
              ],
              true
            ),
            undefined
          ),
          factory.createReturnStatement(factory.createNumericLiteral("0")),
        ],
        true
      )
    );
  }
}