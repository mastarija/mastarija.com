---
name: Haskell generics
less: Harvesting the trees
more: Haskell generics can seem rather abstract and confusing. With a little bit of guidance you will see they are nothing to worry about.
creator: Luka
pubDate: 2021-07-23
---

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque nec tellus bibendum lacus ultricies gravida. Donec odio massa, dapibus quis sollicitudin ultrices, interdum sit amet felis. Fusce in dictum urna, id luctus tortor. Maecenas nec imperdiet dui. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Mauris ut quam molestie, mollis est eu, tristique purus. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Donec condimentum mollis diam in hendrerit. Ut dignissim gravida nibh sagittis cursus. Ut vitae rutrum justo, sit amet pretium est. Fusce ut semper quam. Phasellus metus libero, scelerisque ut efficitur at, feugiat nec massa. Donec eleifend pulvinar nunc nec gravida. Duis tincidunt odio vel lacus efficitur iaculis. Proin bibendum, erat in dictum sagittis, tortor velit porttitor leo, eget aliquet odio tortor id augue.

```haskell
dateMP :: ReadP Natural
dateMP = do
  ms <- count 2 digiP <++ ( (:[]) <$> digiP )
  maybe
    pfail
    ( \ m -> case m of
      _ | m > 12 || m == 0 -> pfail
        | otherwise        -> pure m
    )
    ( readMaybe ms )

dateDP :: ReadP Natural
dateDP = do
  ds <- count 2 digiP <++ ( (:[]) <$> digiP )
  maybe
    pfail
    ( \ d -> case d of
      _ | d > 31 || d == 0 -> pfail
        | otherwise        -> pure d
    )
    ( readMaybe ds )
```

Ut vehicula auctor metus id elementum. In porta, nibh vel auctor ullamcorper, odio sem cursus ipsum, nec elementum justo sem sed dolor. Phasellus convallis nunc eget metus scelerisque ullamcorper. Maecenas scelerisque enim sit amet dui lacinia viverra. Pellentesque quis placerat diam. In faucibus neque sit amet mi pellentesque dapibus. Aenean fermentum dui sit amet sem faucibus, id molestie nibh varius. Maecenas vestibulum enim nunc, quis aliquam nulla tempus blandit. Pellentesque tempus urna et malesuada blandit. Praesent scelerisque quis ante at laoreet. In bibendum sed arcu eu faucibus. Curabitur eros elit, posuere a bibendum in, ornare sed risus. Donec in nisi nibh. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Quisque at metus luctus, iaculis libero a, blandit augue. Vestibulum vel feugiat nibh.

Phasellus sed faucibus tortor. Nullam at tempus ligula, in rutrum dolor. Etiam sed augue convallis, dignissim ipsum sed, vestibulum dolor. Fusce dignissim efficitur purus eget pharetra. Vivamus et tincidunt nisi. Maecenas placerat est arcu, vitae scelerisque ante vestibulum ac. Nullam imperdiet blandit malesuada. Phasellus aliquam, enim nec feugiat pellentesque, dolor nunc gravida arcu, in dignissim nisl tortor non urna. Nullam metus lorem, vestibulum in sapien in, viverra finibus mi. Ut scelerisque aliquet urna quis porttitor. Nullam rhoncus rhoncus arcu, vitae fermentum nisi accumsan ut.

Fusce a varius dui. Duis posuere nulla at eros semper, eget imperdiet justo vestibulum. Nam rhoncus tristique nisl, vel vestibulum mi dapibus eu. Nunc ullamcorper elementum urna sed porta. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur sollicitudin risus interdum arcu ornare rhoncus. Mauris maximus magna sed lectus consequat, a ultricies ante convallis. Etiam nec vestibulum augue. Fusce velit tortor, fringilla nec est quis, blandit vulputate tortor. Suspendisse potenti.
