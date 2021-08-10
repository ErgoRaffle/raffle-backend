package org.ergoplatform.appkit.impl;

import com.google.gson.Gson;
import org.ergoplatform.ErgoBox;
import org.ergoplatform.ErgoLikeTransaction;
import org.ergoplatform.Input;
import org.ergoplatform.appkit.InputBox;
import org.ergoplatform.appkit.Iso;
import org.ergoplatform.appkit.SignedInput;
import org.ergoplatform.appkit.SignedTransaction;
import org.ergoplatform.restapi.client.ErgoTransaction;
import org.ergoplatform.restapi.client.JSON;
import sigmastate.Values;

import java.util.List;
import java.util.stream.Collectors;

public class SignedTransactionImpl implements SignedTransaction {

    private final BlockchainContextImpl _ctx;
    private final ErgoLikeTransaction _tx;
    private final int _txCost;

    public SignedTransactionImpl(BlockchainContextImpl ctx, ErgoLikeTransaction tx, int txCost) {
        _ctx = ctx;
        _tx = tx;
        _txCost = txCost;
    }

    /**
     * Returns underlying {@link ErgoLikeTransaction}
     */
    public ErgoLikeTransaction getTx() {
        return _tx;
    }

    @Override
    public String toString() {
        return "Signed(" + _tx + ")";
    }

    @Override
    public String getId() {
        return _tx.id();
    }

    @Override
    public String toJson(boolean prettyPrint) {
        return toJson(prettyPrint, true);
    }

    @Override
    public String toJson(boolean prettyPrint, boolean formatJson) {
    	ErgoTransaction tx = ScalaBridge.isoErgoTransaction().from(_tx);
    	if (prettyPrint) {
    		tx.getOutputs().forEach(o -> {
    			Values.ErgoTree tree = ScalaBridge.isoStringToErgoTree().to(o.getErgoTree());
    			o.ergoTree(tree.toString());
    		});
    	}
    	Gson gson = (prettyPrint || formatJson) ? JSON.createGson().setPrettyPrinting().create() : _ctx.getApiClient().getGson();
    	String json = gson.toJson(tx);
    	return json;
    }

    @Override
    public List<SignedInput> getSignedInputs() {
        List<Input> inputs = Iso.JListToIndexedSeq(Iso.<Input>identityIso()).from(_tx.inputs());
        List<SignedInput> res = inputs.stream()
                .map(input -> (SignedInput)new SignedInputImpl(this, input)).collect(Collectors.toList());
        return res;
    }

    @Override
    public List<InputBox> getOutputsToSpend() {
        List<ErgoBox> outputs = Iso.JListToIndexedSeq(Iso.<ErgoBox>identityIso()).from(_tx.outputs());
        List<InputBox> res = outputs.stream()
          .map(ergoBox -> (InputBox)new InputBoxImpl(_ctx, ergoBox)).collect(Collectors.toList());
        return res;
    }

    @Override
    public int getCost() {
        return _txCost;
    }
}
