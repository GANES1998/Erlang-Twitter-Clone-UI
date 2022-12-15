import classes from "./formcomp.module.scss";
import {useField} from "formik";
import clsx from "clsx";

const TextArea = ({name, placeholder}) => {
    const [field, meta, helper] = useField(name);

    return (
        <div>
            <textarea
                   className={clsx(classes.formComponent, classes.textArea)}
                   placeholder={placeholder}
                   value={field.value}
                   onChange={(e) => helper.setValue(e.target.value)}
                   rows={5}
            />
        </div>
    )
}

export default TextArea;