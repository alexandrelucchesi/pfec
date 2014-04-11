// View used for finding authorization tokens associated with a contract.
var listTokensWithContractUUID = function (doc) {
    if (doc.type == 'token') {
        emit([doc.contractUUID, doc.value], doc);
    }
}

