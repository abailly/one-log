package aleryo;

import com.fasterxml.jackson.annotation.JsonProperty;
import org.hibernate.validator.constraints.Length;

public class Payment {

    private String cardNumber;

    public Payment() {
        // Jackson deserialization
    }

    public Payment(String cardNumber) {
        this.cardNumber = cardNumber;
    }

    @JsonProperty
    public String getCardNumber() {
        return cardNumber;
    }

}
