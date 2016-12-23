import java.util.List;
import java.util.stream.Stream;

public class Demo {
  public static void main(String[] args) {
    Stream<Integer> nums = Stream.iterate(1, n -> n + 1);
    nums.limit(Integer.parseInt(args[0])).forEach(System.out::println);
  }
}
