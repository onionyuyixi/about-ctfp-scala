
package onion_scalaz

trait Category [=>: [_,_]] extends Compose [=>:]{

    def id[A]:A =>: A

}
