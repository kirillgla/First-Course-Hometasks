Description:
    Equal to Autumn\task8
    Applies selected filter to selected .bmp image.
    Argument order does not matter.
Arguments:
    -i <source_file_name>
        - path to image to be modified. File needs to exist
    -o <destination_file_name>
        - path to desired destination of new image.
    -f <filter_name>
        - name of filter to be applied.
        - possible filters:
            - gauss
                - Gauss' filter 3x3
            - sobelx
                - Soblel's filter on X-axis
            - sobely
                -Sobel's filter on Y-axis
            - greyen
                - change all colours to shades of grey
Examples:
    myInstagram.exe -i C:\sourceImage.bmp -f gauss -o C:\destinationImage.bmp
    myInstagram.exe -f greyen -o destinationImage.bmp -i sourceImage.bmp
