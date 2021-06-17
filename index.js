import { Elm } from './src/PortsPos.elm'

const BCHJS = require("@psf/bch-js")
let bchjs = new BCHJS() // Defaults to BCHN network.

var app = Elm.PortsPos.init({
    node: document.getElementById("elm-node")
});

app.ports.getCashAddress.subscribe(_ => {
    // create mnemonic
    //let mnemonic = bchjs.Mnemonic.generate(128);
    let mnemonic = localStorage.getItem('wallet');
    console.log(mnemonic)
    restoreWalletFromMnenomic(mnemonic).then(cashAddress => {
        app.ports.cashAddressReceiver.send(cashAddress)

    })
    // create seed buffer from mnemonic

})

app.ports.getBchPrice.subscribe(_ => {
    bchjs.Price.getBchUsd().then(current => {
        app.ports.bchPriceReceiver.send(current)
    })
})

let restoreWalletFromMnenomic = mnenomic => {
    if (mnenomic) {
        return bchjs.Mnemonic.toSeed(mnemonic).then(seedBuffer => {
            // create HDNode from seed buffer
            let hdNode = bchjs.HDNode.fromSeed(seedBuffer);
            // derive hardened child HDNode
            let account = bchjs.HDNode.deriveHardened(hdNode, 0);
            let cashAddress = bchjs.HDNode.toCashAddress(account)
            return cashAddress
        })
    }
    return Promise.reject("No Wallet found in localStorage")
}
