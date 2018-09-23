package aleryo;

import aleryo.Payment;
import aleryo.PaymentResult;
import com.google.common.base.Optional;
import com.codahale.metrics.annotation.Timed;

import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.Consumes;
import javax.ws.rs.core.MediaType;
import java.util.Random;
import java.util.concurrent.atomic.AtomicLong;

@Path("/payment")
@Produces(MediaType.APPLICATION_JSON)
public class PaymentResource {
    private final Random random = new Random();
    private final AtomicLong counter;

    public PaymentResource() {
        this.counter = new AtomicLong();
    }

    @POST
    @Consumes({MediaType.APPLICATION_JSON})
    @Timed
    public PaymentResult sayHello(Payment payment) {
        failRandomly();
        return new PaymentResult(counter.incrementAndGet(),
                payment.checkCardNumber());
    }

    private void failRandomly() {
        if (random.nextInt(5) == 0) {
            throw new RuntimeException("something's wrong in the payment system");
        }
    }
}
