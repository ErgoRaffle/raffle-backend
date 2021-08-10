package org.ergoplatform.appkit

import org.ergoplatform.ErgoBox
import sigmastate.interpreter.ContextExtension

/** Input ErgoBox paired with context variables (aka ContextExtensions).
 *
 * @param box       an instance of ErgoBox which is used as an input of the transaction.
 * @param extension a set of context variables necessary to satisfy the box's
 *                  guarding proposition.
 *                  This extension is also saved in the corresponding
 *                  [[org.ergoplatform.Input]] instance of the signed transaction.
 */
case class ExtendedInputBox(
  box: ErgoBox,
  extension: ContextExtension
)
